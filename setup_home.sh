#!/bin/bash

CONFIG="$( cd "$(dirname "$0")" ; pwd -P )"

cd ~

mkdir ~/.oldhome

# Backup existing configuration
if [[ -f "~/.bashrc" ]]; then
    mv ~/.bashrc ~/.oldhome
fi
if [[ -f "~/.bash_logout" ]]; then
    mv ~/.bash_logout ~/.oldhome
fi
if [[ -f "~/.emacs" ]]; then
    mv ~/.emacs ~/.oldhome
fi

# Setup soft links to point to git config
ln -s $CONFIG/home/.bashrc
ln -s $CONFIG/home/.bash_logout
ln -s $CONFIG/home/.bash_env
ln -s $CONFIG/home/.bash_aliases
ln -s $CONFIG/home/.bash_custom
ln -s $CONFIG/home/.fehbg
ln -s $CONFIG/home/.gitignore_global
ln -s $CONFIG/home/.profile
ln -s $CONFIG/home/.Xdefaults
ln -s $CONFIG/home/.xinitrc
ln -s $CONFIG/home/.xinputrc
ln -s $CONFIG/home/.Xresources
ln -s $CONFIG/home/.xsession

# EMACS
#   For emacs25
ln -s $CONFIG/emacs/emacs25 $CONFIG/emacs

ln -s $CONFIG/emacs/.emacs
ln -s $CONFIG/emacs/.emacs-custom.el
ln -s $CONFIG/emacs/emacs

