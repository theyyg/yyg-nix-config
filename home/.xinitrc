# swap the caps-lock and ctrl keys on the keyboard
setxkbmap -layout us -option ctrl:swapcaps
# setxkbmap -option "caps:ctrl"
# setxkbmap -option "shift:both_capslock"
#
# swap the windows and alt keys on the keyboard
# setxkbmap -layout us -option altwin:swap_lalt_lwin

gsettings set org.gnome.nautilus.desktop trash-icon-visible false
gsettings set org.gnome.shell.extensions.desktop-icons show-home false
gsettings set org.gnome.shell.extensions.desktop-icons show-trash false

eval $(cat ~/opt/yyg-nix-config/.fehbg)

