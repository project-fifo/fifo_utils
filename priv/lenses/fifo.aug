(*
Module: FiFo
  FiFo config files, based on Simplewar (aka copied)

Author: Raphael Pinson <raphink@gmail.com> / Heinz N. Gies <heinz@project-fifo.net>

About: License
   This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
   To be documented

About: Examples
   The <Test_Simplevars> file contains various examples and tests.
*)

module FiFo =

autoload xfm

(* Variable: to_comment_re
   The regexp to match the value *)
let to_comment_re =
     let to_comment_squote = /'[^\n']*'/
  in let to_comment_dquote = /"[^\n"]*"/
  in let to_comment_noquote = /[^\n \t'"#][^\n#]*[^\n \t#]|[^\n \t'"#]/
  in to_comment_squote | to_comment_dquote | to_comment_noquote

(* View: entry *)
let entry =
     let some_value = Sep.space_equal . store to_comment_re
     (* Avoid ambiguity in tree by making a subtree here *)
  in let empty_value = [del /[ \t]*=/ "="] . store ""
  in [ Util.indent . key Rx.word
            . (some_value? | empty_value)
            . (Util.eol | Util.comment_eol) ]

(* View: lns *)
let lns = (Util.empty | Util.comment | entry)*

(* Variable: filter *)
let filter = incl "/data/howl/etc/howl.conf"
           . incl "/data/sniffle/etc/sniffle.conf"
           . incl "/data/snarl/etc/snarl.conf"
           . incl "/data/leo_manager/etc/leo_manager.conf"
           . incl "/data/leo_gateway/etc/leo_gateway.conf"
           . incl "/data/leo_storage/etc/leo_storage.conf"

let xfm = transform lns filter