#!/bin/bash

config=`pwd`

cd ~

mkdir ~/.oldhome

mv ~/.bashrc ~/.oldhome
ln -s $config/bash/.bashrc

mv ~/.bash_logout ~/.oldhome
ln -s $config/bash/.bash_logout

ln -s $config/bash/.bash_env

ln -s $config/bash/.bash_aliases

mv ~/.emacs ~/.oldhome
ln -s $config/emacs/.emacs
ln -s $config/emacs/.emacs-custom.el
ln -s $config/emacs/emacs
