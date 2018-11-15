#!/bin/bash

if [[ -z "$1" ]]; then
    WORKSPACE=$(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
else
    WORKSPACE=$1    
fi
echo "i3-msg workspace $WORKSPACE"
i3-msg workspace $WORKSPACE
