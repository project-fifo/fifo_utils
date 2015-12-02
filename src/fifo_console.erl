-module(fifo_console).

-export([hdr/1, fields/2, print_config/2]).

-export([get_ring/1, aae_status/1]).

-export([status/1, join/1, leave/1, remove/1, down/1, vnode_status/1,
         ringready/1, staged_join/1, reip/1, cluster_info/1]).

hdr(F) ->
    hdr_lines(lists:reverse(F), {"~n", [], "~n", []}).


hdr_lines([{N, n} | R], {Fmt, Vars, FmtLs, VarLs}) ->
    hdr_lines(R, {
                "~20s " ++ Fmt,
                [N | Vars],
                "~20c " ++ FmtLs,
                [$- | VarLs]});

hdr_lines([{N, S}|R], {Fmt, Vars, FmtLs, VarLs}) ->
    %% there is a space that matters here ---------v
    hdr_lines(R, {
                [$~ | integer_to_list(S) ++ [$s, $\s | Fmt]],
                [N | Vars],
                [$~ | integer_to_list(S) ++ [$c, $\s | FmtLs]],
                [$- | VarLs]});

hdr_lines([], {Fmt, Vars, FmtL, VarLs}) ->
    io:format(Fmt, Vars),
    io:format(FmtL, VarLs).


fields(F, Vs) ->
    fields(lists:reverse(F),
           lists:reverse(Vs),
           {"~n", []}).

print_config(Prefix, SubPrefix) ->
    Fmt = [{"Key", 30}, {"Value", 50}],
    hdr(Fmt),
    PrefixB = fifo_utils:ensure_bin(Prefix),
    SubPrefixB = fifo_utils:ensure_bin(SubPrefix),
    PrintFn = fun({K, [V|_]}, _) ->
                      fields(Fmt, [key(Prefix, SubPrefix, K), V])
              end,
    riak_core_metadata:fold(PrintFn, ok, {PrefixB, SubPrefixB}).


fields([{_, n}|R], [V | Vs], {Fmt, Vars}) when is_list(V)
                                     orelse is_binary(V) ->
    fields(R, Vs, {"~s " ++ Fmt, [V | Vars]});

fields([{_, n}|R], [V | Vs], {Fmt, Vars}) ->
    fields(R, Vs, {"~p " ++ Fmt, [V | Vars]});

fields([{_, S}|R], [V | Vs], {Fmt, Vars}) when is_list(V)
                                     orelse is_binary(V) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$s, $\s | Fmt]], [V | Vars]});


fields([{_, S}|R], [V | Vs], {Fmt, Vars}) when is_integer(V) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$b, $\s | Fmt]], [V | Vars]});

fields([{_, S}|R], [V | Vs], {Fmt, Vars}) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$p, $\s | Fmt]], [V | Vars]});

fields([], [], {Fmt, Vars}) ->
    io:format(Fmt, Vars).


key(Prefix, SubPrefix, Key) ->
    io_lib:format("~s.~s.~s", [Prefix, SubPrefix, Key]).

get_ring([]) ->
    {ok, RingData} = riak_core_ring_manager:get_my_ring(),
    {_S, CHash} = riak_core_ring:chash(RingData),
    io:format("Hash                "
              "                    "
              "          "
              " Node~n"),
    io:format("--------------------"
              "--------------------"
              "----------"
              " ---------------~n", []),
    lists:map(fun({K, H}) ->
                      io:format("~50b ~-40s~n", [K, H])
              end, CHash),
    ok.

aae_status({System, Name}) ->
    ExchangeInfo = riak_core_entropy_info:compute_exchange_info(System),
    io:format("~s~n~n", [Name]),
    aae_exchange_status(ExchangeInfo),
    io:format("~n"),
    aae_tree_status(System),
    io:format("~n"),
    aae_repair_status(ExchangeInfo).


%%%===================================================================
%%% From riak_kv/riak_console
%%%===================================================================

join([NodeStr]) ->
    join(NodeStr, fun riak_core:join/1,
         "Sent join request to ~s~n", [NodeStr]).

staged_join([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    join(NodeStr, fun riak_core:staged_join/1,
         "Success: staged join request for ~p to ~p~n", [node(), Node]).

join(NodeStr, JoinFn, SuccessFmt, SuccessArgs) ->
    try
        case JoinFn(NodeStr) of
            ok ->
                io:format(SuccessFmt, SuccessArgs),
                ok;
            {error, not_reachable} ->
                io:format("Node ~s is not reachable!~n", [NodeStr]),
                error;
            {error, different_ring_sizes} ->
                io:format("Failed: ~s has a different ring_creation_size~n",
                          [NodeStr]),
                error;
            {error, unable_to_get_join_ring} ->
                io:format("Failed: Unable to get ring from ~s~n", [NodeStr]),
                error;
            {error, not_single_node} ->
                io:format("Failed: This node is already a member of a "
                          "cluster~n"),
                error;
            {error, self_join} ->
                io:format("Failed: This node cannot join itself in a "
                          "cluster~n"),
                error;
            {error, _} ->
                io:format("Join failed. Try again in a few moments.~n", []),
                error
        end
    catch
        Exception:Reason ->
            lager:error("Join failed ~p:~p", [Exception, Reason]),
            io:format("Join failed, see log for details~n"),
            error
    end.

leave([]) ->
    try
        case riak_core:leave() of
            ok ->
                io:format("Success: ~p will shutdown after handing off "
                          "its data~n", [node()]),
                ok;
            {error, already_leaving} ->
                io:format("~p is already in the process of leaving the "
                          "cluster.~n", [node()]),
                ok;
            {error, not_member} ->
                io:format("Failed: ~p is not a member of the cluster.~n",
                          [node()]),
                error;
            {error, only_member} ->
                io:format("Failed: ~p is the only member.~n", [node()]),
                error
        end
    catch
        Exception:Reason ->
            lager:error("Leave failed ~p:~p", [Exception, Reason]),
            io:format("Leave failed, see log for details~n"),
            error
    end.

remove([Node]) ->
    try
        case riak_core:remove(list_to_atom(Node)) of
            ok ->
                io:format("Success: ~p removed from the cluster~n", [Node]),
                ok;
            {error, not_member} ->
                io:format("Failed: ~p is not a member of the cluster.~n",
                          [Node]),
                error;
            {error, only_member} ->
                io:format("Failed: ~p is the only member.~n", [Node]),
                error
        end
    catch
        Exception:Reason ->
            lager:error("Remove failed ~p:~p", [Exception, Reason]),
            io:format("Remove failed, see log for details~n"),
            error
    end.

down([Node]) ->
    try
        case riak_core:down(list_to_atom(Node)) of
            ok ->
                io:format("Success: ~p marked as down~n", [Node]),
                ok;
            {error, is_up} ->
                io:format("Failed: ~s is up~n", [Node]),
                error;
            {error, not_member} ->
                io:format("Failed: ~p is not a member of the cluster.~n",
                          [Node]),
                error;
            {error, only_member} ->
                io:format("Failed: ~p is the only member.~n", [Node]),
                error
        end
    catch
        Exception:Reason ->
            lager:error("Down failed ~p:~p", [Exception, Reason]),
            io:format("Down failed, see log for details~n"),
            error
    end.

-spec(status([]) -> ok).
%% TODO: This was the original replaced it with some own code
%% status([]) ->
%%     try
%%         %% Stats = riak_kv_status:statistics(),
%% 	%% StatString = format_stats(Stats,
%%         %%             ["-------------------------------------------\n",
%% 	%%	     io_lib:format("1-minute stats for ~p~n",[node()])]),
%% 	%% io:format("~s\n", [StatString])
%%         ok
%%     catch
%%         Exception:Reason ->
%%             lager:error("Status failed ~p:~p", [Exception,
%%                     Reason]),
%%             io:format("Status failed, see log for details~n"),
%%             error
%%     end.
status([]) ->
    case riak_core_status:transfers() of
        {[], []} ->
            io:format("The cluster is fine!~n"),
            ok;
        {[], H} ->
            io:format("There are ~p handoffs pending!~n", [length(H)]),
            error;
        {S, []} ->
            io:format("There are ~p servers down!~n", [length(S)]),
            error;
        {S, H} ->
            io:format("There are ~p handoffs pending and ~p servers down!~n",
                      [length(H), length(S)]),
            error
    end.

-spec(vnode_status([]) -> ok).
vnode_status([]) ->
    try
%%        case riak_kv_status:vnode_status() of
%%            [] ->
%%                io:format("There are no active vnodes.~n");
%%            Statuses ->
%%                io:format("~s~n-------------------------------------------~n~n",
%%                          ["Vnode status information"]),
%%                print_vnode_statuses(lists:sort(Statuses))
%%        end
        ok
    catch
        Exception:Reason ->
            lager:error("Backend status failed ~p:~p", [Exception,
                    Reason]),
            io:format("Backend status failed, see log for details~n"),
            error
    end.

reip([OldNode, NewNode]) ->
    try
        %% reip is called when node is down (so riak_core_ring_manager is not running),
        %% so it has to use the basic ring operations.
        %%
        %% Do *not* convert to use riak_core_ring_manager:ring_trans.
        %%
        case application:load(riak_core) of
            %% a process, such as cuttlefish, may have already loaded riak_core
            {error,{already_loaded,riak_core}} -> ok;
            ok -> ok
        end,
        RingStateDir = app_helper:get_env(riak_core, ring_state_dir),
        {ok, RingFile} = riak_core_ring_manager:find_latest_ringfile(),
        BackupFN = filename:join([RingStateDir, filename:basename(RingFile)++".BAK"]),
        {ok, _} = file:copy(RingFile, BackupFN),
        io:format("Backed up existing ring file to ~p~n", [BackupFN]),
        Ring = riak_core_ring_manager:read_ringfile(RingFile),
        NewRing = riak_core_ring:rename_node(Ring, OldNode, NewNode),
        ok = riak_core_ring_manager:do_write_ringfile(NewRing),
        io:format("New ring file written to ~p~n",
            [element(2, riak_core_ring_manager:find_latest_ringfile())])
    catch
        Exception:Reason ->
            io:format("Reip failed ~p:~p", [Exception, Reason]),
            error
    end.

%% Check if all nodes in the cluster agree on the partition assignment
-spec(ringready([]) -> ok | error).
ringready([]) ->
    try
        case riak_core_status:ringready() of
            {ok, Nodes} ->
                io:format("TRUE All nodes agree on the ring ~p\n", [Nodes]);
            {error, {different_owners, N1, N2}} ->
                io:format("FALSE Node ~p and ~p list different partition owners\n", [N1, N2]),
                error;
            {error, {nodes_down, Down}} ->
                io:format("FALSE ~p down.  All nodes need to be up to check.\n", [Down]),
                error
        end
    catch
        Exception:Reason ->
            lager:error("Ringready failed ~p:~p", [Exception,
                    Reason]),
            io:format("Ringready failed, see log for details~n"),
            error
    end.

cluster_info([OutFile|Rest]) ->
    try
        case lists:reverse(atomify_nodestrs(Rest)) of
            [] ->
                cluster_info:dump_all_connected(OutFile);
            Nodes ->
                cluster_info:dump_nodes(Nodes, OutFile)
        end
    catch
        error:{badmatch, {error, eacces}} ->
            io:format("Cluster_info failed, permission denied writing to ~p~n", [OutFile]);
        error:{badmatch, {error, enoent}} ->
            io:format("Cluster_info failed, no such directory ~p~n", [filename:dirname(OutFile)]);
        error:{badmatch, {error, enotdir}} ->
            io:format("Cluster_info failed, not a directory ~p~n", [filename:dirname(OutFile)]);
        Exception:Reason ->
            lager:error("Cluster_info failed ~p:~p",
                [Exception, Reason]),
            io:format("Cluster_info failed, see log for details~n"),
            error
    end.

%% format_stats([], Acc) ->
%%     lists:reverse(Acc);
%% format_stats([{Stat, V}|T], Acc) ->
%%     format_stats(T, [io_lib:format("~p : ~p~n", [Stat, V])|Acc]).

atomify_nodestrs(Strs) ->
    lists:foldl(fun("local", Acc) -> [node()|Acc];
                   (NodeStr, Acc) -> try
                                         [list_to_existing_atom(NodeStr)|Acc]
                                     catch error:badarg ->
                                         io:format("Bad node: ~s\n", [NodeStr]),
                                         Acc
                                     end
                end, [], Strs).

%% print_vnode_statuses([]) ->
%%     ok;
%% print_vnode_statuses([{VNodeIndex, StatusData} | RestStatuses]) ->
%%     io:format("VNode: ~p~n", [VNodeIndex]),
%%     print_vnode_status(StatusData),
%%     io:format("~n"),
%%     print_vnode_statuses(RestStatuses).

%% print_vnode_status([]) ->
%%     ok;
%% print_vnode_status([{backend_status,
%%                      Backend,
%%                      StatusItem} | RestStatusItems]) ->
%%     if is_binary(StatusItem) ->
%%             StatusString = binary_to_list(StatusItem),
%%             io:format("Backend: ~p~nStatus: ~n~s~n",
%%                       [Backend, string:strip(StatusString)]);
%%        true ->
%%             io:format("Backend: ~p~nStatus: ~n~p~n",
%%                       [Backend, StatusItem])
%%     end,
%%     print_vnode_status(RestStatusItems);
%% print_vnode_status([StatusItem | RestStatusItems]) ->
%%     if is_binary(StatusItem) ->
%%             StatusString = binary_to_list(StatusItem),
%%             io:format("Status: ~n~s~n",
%%                       [string:strip(StatusString)]);
%%        true ->
%%             io:format("Status: ~n~p~n", [StatusItem])
%%     end,
%%     print_vnode_status(RestStatusItems).

%%%===================================================================
%%% Private
%%%===================================================================


aae_exchange_status(ExchangeInfo) ->
    io:format("~s~n", [string:centre(" Exchanges ", 79, $=)]),
    io:format("~-49s  ~-12s  ~-12s~n", ["Index", "Last (ago)", "All (ago)"]),
    io:format("~79..-s~n", [""]),
    [begin
         Now = os:timestamp(),
         LastStr = format_timestamp(Now, LastTS),
         AllStr = format_timestamp(Now, AllTS),
         io:format("~-49b  ~-12s  ~-12s~n", [Index, LastStr, AllStr]),
         ok
     end || {Index, LastTS, AllTS, _Repairs} <- ExchangeInfo],
    ok.

aae_repair_status(ExchangeInfo) ->
    io:format("~s~n", [string:centre(" Keys Repaired ", 79, $=)]),
    io:format("~-49s  ~s  ~s  ~s~n", ["Index",
                                      string:centre("Last", 8),
                                      string:centre("Mean", 8),
                                      string:centre("Max", 8)]),
    io:format("~79..-s~n", [""]),
    [begin
         io:format("~-49b  ~s  ~s  ~s~n",
                   [Index,
                    string:centre(integer_to_list(Last), 8),
                    string:centre(integer_to_list(Mean), 8),
                    string:centre(integer_to_list(Max), 8)]),
         ok
     end || {Index, _, _, {Last,_Min,Max,Mean}} <- ExchangeInfo],
    ok.

aae_tree_status(System) ->
    TreeInfo = riak_core_entropy_info:compute_tree_info(System),
    io:format("~s~n", [string:centre(" Entropy Trees ", 79, $=)]),
    io:format("~-49s  Built (ago)~n", ["Index"]),
    io:format("~79..-s~n", [""]),
    [begin
         Now = os:timestamp(),
         BuiltStr = format_timestamp(Now, BuiltTS),
         io:format("~-49b  ~s~n", [Index, BuiltStr]),
         ok
     end || {Index, BuiltTS} <- TreeInfo],
    ok.

format_timestamp(_Now, undefined) ->
    "--";
format_timestamp(Now, TS) ->
    riak_core_format:human_time_fmt("~.1f", timer:now_diff(Now, TS)).
