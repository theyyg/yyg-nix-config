#!/bin/bash

CONFIG="$( cd "$(dirname "$0")" ; pwd -P )"
pushd `pwd`

cd ~
OLD="~/.oldhome"
mkdir -p $OLD

move_and_link() {
    if [ $# -eq 4 ]; then
		# Backup directory
		BACK="$1"

		# Source file directory
		FOLDER="$2"

		# Destination directory of soft link
		DEST="$3"

		# File name
		FILE="$4"

		if [ -f "$DEST/$FILE" ] || [ -L "$DEST/$FILE" ]; then
			echo "> mv $DEST/$FILE $BACK/$FILE"
			mv $DEST/$FILE $BACK/$FILE
			echo "- $FILE has been backed up to $BACK/$FILE"
		fi

		if [ ! -f "$DEST/$FILE" ] || [ ! -L "$DEST/$FILE" ]; then
			echo "> ln -s $FOLDER/$FILE $DEST/$FILE"
			ln -s $FOLDER/$FILE $DEST/$FILE
			echo "- $FILE has been linked to ($DEST/$FILE)"
		fi
    fi
    echo ""
}

move_and_link $OLD $CONFIG/home $HOME .bashrc
move_and_link $OLD $CONFIG/home $HOME .bash_logout
move_and_link $OLD $CONFIG/home $HOME .bash_env
move_and_link $OLD $CONFIG/home $HOME .bash_aliases
move_and_link $OLD $CONFIG/home $HOME .bash_custom
move_and_link $OLD $CONFIG/home $HOME .fehbg
move_and_link $OLD $CONFIG/home $HOME .gitignore_global
move_and_link $OLD $CONFIG/home $HOME .profile
move_and_link $OLD $CONFIG/home $HOME .Xdefaults
move_and_link $OLD $CONFIG/home $HOME .xinitrc
# move_and_link $OLD $CONFIG/home $HOME .xinputrc
move_and_link $OLD $CONFIG/home $HOME .Xresources
move_and_link $OLD $CONFIG/home $HOME .xsession

if [ ! -L "$CONFIG/emacs" ]; then
	ln -s $CONFIG/emacs25 $CONFIG/emacs
fi

move_and_link $OLD $CONFIG/emacs $HOME .emacs
move_and_link $OLD $CONFIG/emacs $HOME .emacs-custom.el
# move_and_link $OLD $CONFIG $HOME emacs

popd

gsettings set org.gnome.desktop.background show-desktop-icons true
