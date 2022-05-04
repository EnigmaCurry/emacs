#!/bin/sh
# adapted from https://github.com/susam/emfy/blob/main/em
if [ "$#" -eq 0 ]
then
    if emacsclient -cn 2> /dev/null
    then
        echo "Opening a new frame in an existing Emacs server ..." >&2
    else
        echo "Opening a new Emacs process ..." >&2
        nohup emacs "$@" > /dev/null 2>&1 &
    fi
elif emacsclient -n "$@" 2> /dev/null
then
    echo "Opened $@ in Emacs server in existing frame ..." >&2
else
    echo "Opening $@ in a new Emacs process ..." >&2
    nohup emacs "$@" > /dev/null 2>&1 &
fi
