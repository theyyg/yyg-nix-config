#!/bin/bash

DESKTOP=0

CONFIG="$( cd "$(dirname "$0")" ; pwd -P )"
pushd `pwd`

cd $HOME
OLD=$HOME/.oldhome
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
move_and_link $OLD $CONFIG/home $HOME .gitignore_global
move_and_link $OLD $CONFIG/home $HOME .profile

if [ $DESKTOP -ne 0 ]; then
   move_and_link $OLD $CONFIG/home $HOME .fehbg
   move_and_link $OLD $CONFIG/home $HOME .Xdefaults
   move_and_link $OLD $CONFIG/home $HOME .xinitrc
   move_and_link $OLD $CONFIG/home $HOME .xinputrc
   move_and_link $OLD $CONFIG/home $HOME .Xresources
   move_and_link $OLD $CONFIG/home $HOME .xsession
fi

if [ ! -L "$CONFIG/emacs" ]; then
	ln -s $CONFIG/emacs27 $CONFIG/emacs
fi

# Replaced by the org-config emacs 
# move_and_link $OLD $CONFIG/emacs $HOME .emacs
# move_and_link $OLD $CONFIG/emacs $HOME .emacs-custom.el
# move_and_link $OLD $CONFIG $HOME emacs
move_and_link $OLD $CONFIG/org-config $HOME .emacs.d

if [ $DESKTOP -ne 0 ]; then
   move_and_link $OLD $CONFIG/ $HOME .i3
   if [ ! -L "$CONFIG/bin/i3-name" ]; then
       ln -s $CONFIG/.i3/i3-container-name.sh $CONFIG/bin/i3-name
   fi
   if [ ! -L "$CONFIG/bin/i3-sh" ]; then
       ln -s $CONFIG/.i3/i3-container-name.sh $CONFIG/bin/i3-sh
   fi
   if [ ! -L "$CONFIG/bin/i3-ws" ]; then
       ln -s $CONFIG/.i3/i3-create-workspace.sh $CONFIG/bin/i3-ws
   fi
fi

popd

if [ $DESKTOP -ne 0 ]; then
gsettings set org.gnome.desktop.background show-desktop-icons true
fi
