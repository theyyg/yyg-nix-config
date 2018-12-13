#!/bin/bash
# i3-container-name.sh

name="FOO"
action="__fork__"

usage() {
    echo "usage: $(basename $0) NAME" >&2;
    exit 2
}

while true; do case "$1" in
    __*__)  action="$1";    break ;;
    "")  break                    ;;
    -*)  usage                    ;;
    *)   name="$1"; shift 1       ;;
esac done

case "$action" in
    __sleep__)
        sleep 10000000000;
        ;;
    __fork__)
        $0 "$name" __exec__ &
        ;;
    __exec__)
        exec -a "$name" urxvt -title "$name" -e "$0" __sleep__; break ;;
    *)
        usage
        ;;
esac
