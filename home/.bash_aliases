#!/bin/bash

# Program aliases
alias chrome=chromium-browser

alias less='less --RAW-CONTROL-CHARS'
export LS_OPTS='--color=auto'
alias ls='ls ${LS_OPTS}'

alias clc='clear'

ec() {
    if [[ "$#" == "0" ]]; then
	emacsclient -nc -e '(switch-to-buffer nil)' $*
    else
	emacsclient -nc $*
    fi
}

ed() {
    emacs --daemon
}

emacs-kill() {
    emacsclient -e '(kill-emacs)'
}

alias gs="git status"
alias gsp="git status --porcelain"
alias gl="git log --graph --pretty='format:%C(yellow)%h %Cgreen(%><(20)%an)%Cblue [%>(31)%ad / %<(14)%cr]%Cred %>(42,ltrunc)%d%Creset %n          %<(115,trunc)%s %n'"
alias gcp="git cherry-pick"
alias gco="git checkout"
alias grh="git reset --hard"
alias gpf="git push origin HEAD:refs/for/master"
alias gpw="git push origin HEAD:refs/for/master%wip"
alias gpd="git push origin HEAD:refs/for/master%wip"
alias gpr="git push origin HEAD:refs/for/master%ready"
alias gdc="git diff --compact-summary HEAD~..HEAD"
gcf() {
    # clang-format all changed files
    CHANGED_FILES=`git diff --cached --name-only | grep -e ".*\.[(h)|(cc)]"`
    if [ ! -z "$CHANGED_FILES" ] ; then
	clang-format -i $CHANGED_FILES
	echo ">> clang-format executed on staged files:"
	echo $CHANGED_FILES | tr ' ' '\n' | xargs -d ' ' echo
    fi

    # clang-format all changed files
    CHANGED_FILES=`git diff --name-only HEAD~..HEAD | grep -e ".*\.[(h)|(cc)]"`
    if [ ! -z "$CHANGED_FILES" ] ; then
	clang-format -i $CHANGED_FILES
	echo ">> clang-format executed on changed files:"
	echo $CHANGED_FILES | tr ' ' '\n' | xargs -d ' ' echo
    fi
}
