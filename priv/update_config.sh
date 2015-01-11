#!/usr/bin/env bash

sed=sed
grep=grep
if [ -f /opt/local/bin/sed ]
then
  sed=/opt/local/bin/sed
fi
if [ -f /opt/local/bin/grep ]
then
  grep=/opt/local/bin/grep
fi

example=$1
old=$2
get() {
    var=$2
    conf=$1

    res=$($grep "^[ ]*${var}[ ]*=" $conf | $sed "s/.*${var}[ ]*=[ ]*//")
    if [ -z "${res}" ]
    then
        exit 1
    fi
    echo "${res}"
}


while read line
do
    if echo "${line}" | $grep -v '^#' | $grep -v '^$' > /dev/null
    then
        key=$(echo "${line}" | $sed 's/[ ]*=.*//')
        if val=$(get ${old} ${key})
        then
            echo "${key} = ${val}"
        else
            echo "${line}"
        fi
    elif echo "${line}" | $grep '^#\+[ ]*.\+=.\+' > /dev/null
    then
         ## If the line looks like a commented value try to find that
         ## git puvalue in the old config to see if we need to uncomment it
         key=$(echo "${line}" | $sed 's/[ ]*=.*$//' | $sed 's/^#*[ ]*//')
         if val=$(get ${old} ${key})
         then
             echo "${key} = ${val}"
         else
             echo "${line}"
         fi
    else
        echo "${line}"

    fi
done < "${example}"
