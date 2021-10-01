#!/bin/bash

# Run my global bash_aliases

# Program aliases
alias chrome=chromium-browser

alias less='less --RAW-CONTROL-CHARS'
export LS_OPTS='--color=auto'
alias ls='ls ${LS_OPTS}'

alias clc='clear'

# make sudo work with aliases
alias sudo="sudo "

alias pip="python3 -m pip"

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

alias rsync="repo sync -c -j8 -q --no-tags --optimized-fetch"
alias rsyncvip="repo sync -c -j12 --no-tags --optimized-fetch --no-clone-bundle \"$@\""
alias rinfo="repo info ."

alias bearmake="bear -- 'make -C `pwd` -f build/core/main.mk -j 24'"

# fastboot devices
fd() {
    fastboot devices
}


feu() {
    fastboot erase userdata
}


alias lockscreen="gnome-screensaver-command -l"
alias suspend="systemctl suspend"
alias hibernate="systemctl hibernate"
#alias lockscreen="i3lock -c 000000"

export BUG_DIR=$HOME/dev/bugs

cd_bug(){
    cd $BUG_DIR
}

bug()
{
    cd $BUG_DIR

    if [[ ! -z "$1" ]]; then
	if [[ ! -d "$1" ]]; then
	    mkdir -p $1
	fi
	cd $1
    fi
}

countdown() {
    for j in `seq $1 -1 1`;
    do
	printf "\r $j seconds remain        "		
	sleep 1
    done
    printf "\rCountdown completed ($1 sec) \n"
}

ctags-index() {
    ctags -e -R .
}

rlistprojects() {
    repo forall -c 'echo "$REPO_PATH -- $REPO_PROJECT"'
}

emacs-kill() {
    emacsclient -e '(kill-emacs)'
}

mkcd() {
    mkdir $1
    cd $1
}

#                     brightness (int)    : min=0 max=255 step=1 default=128 value=128
#                       contrast (int)    : min=0 max=255 step=1 default=128 value=128
#                     saturation (int)    : min=0 max=255 step=1 default=128 value=128
# white_balance_temperature_auto (bool)   : default=1 value=1
#                           gain (int)    : min=0 max=255 step=1 default=0 value=42
#           power_line_frequency (menu)   : min=0 max=2 default=2 value=2
#      white_balance_temperature (int)    : min=2000 max=7500 step=1 default=4000 value=4736 flags=inactive
#                      sharpness (int)    : min=0 max=255 step=1 default=128 value=128
#         backlight_compensation (int)    : min=0 max=1 step=1 default=0 value=0
#                  exposure_auto (menu)   : min=0 max=3 default=3 value=3
#              exposure_absolute (int)    : min=3 max=2047 step=1 default=250 value=333 flags=inactive
#         exposure_auto_priority (bool)   : default=0 value=1
#                   pan_absolute (int)    : min=-36000 max=36000 step=3600 default=0 value=0
#                  tilt_absolute (int)    : min=-36000 max=36000 step=3600 default=0 value=0
#                 focus_absolute (int)    : min=0 max=255 step=5 default=0 value=0 flags=inactive
#                     focus_auto (bool)   : default=1 value=1
#                  zoom_absolute (int)    : min=100 max=400 step=1 default=100 value=100	
cam-zoom() {
    # apt install v4l-utils
    DEVICE="/dev/video0"
    
    if [[ -n "$1" ]]; then
	v4l2-ctl -d $DEVICE --set-ctrl=zoom_absolute=100
	
	if [[ "$1" == "near" ]]; then
	    v4l2-ctl -d $DEVICE --set-ctrl=zoom_absolute=400
	elif [[ "$1" == "far" ]]; then
	    v4l2-ctl -d $DEVICE --set-ctrl=zoom_absolute=100
	else
	    v4l2-ctl -d $DEVICE --set-ctrl=zoom_absolute=$1
	fi
	
    else
	current_zoom=`v4l2-ctl -d $DEVICE --get-ctrl=zoom_absolute`
    fi
}

cam-focus() {
    # apt install v4l-utils
    DEVICE="/dev/video0"
    
    if [[ -n "$1" ]]; then
	v4l2-ctl -d $DEVICE --set-ctrl=focus_auto=0
	
	if [[ "$1" == "near" ]]; then
	    v4l2-ctl -d $DEVICE --set-ctrl=focus_absolute=255
	elif [[ "$1" == "far" ]]; then
	    v4l2-ctl -d $DEVICE --set-ctrl=focus_absolute=0
	elif [[ "$1" == "auto" ]]; then
	    v4l2-ctl -d $DEVICE --set-ctrl=focus_auto=1
	else
	    v4l2-ctl -d $DEVICE --set-ctrl=focus_absolute=$1
	fi
	
    else
	current_focus=`v4l2-ctl -d $DEVICE --get-ctrl=focus_absolute`
    fi
}

refresh-term() {
    xrdb -load ~/.Xresources
}

comp-setup() {
    compton --config ~/opt/yyg-nix-config/.config/compton.conf --blur-background -f -i 0.9 -e 0.7 --active-opacity 1.0 &
}

transparent-desktop() {
    comp-setup
}

gnome-disable-desktop() {
    gsettings set org.gnome.desktop.background show-desktop-icons false
}

obs-loopback() {
    # Add a virtual camera for loopback
    sudo modprobe v4l2loopback devices=1 video_nr=10 card_label="OBS Cam" exclusive_caps=1
    # Add a virtual audio card for loopback
    sudo modprobe snd-aloop index=10 id="OBS Mic"
    # Rename devices in pulse audio so that they're easier to identify
    pacmd update-source-proplist alsa_input.platform-snd_aloop.0.analog-stereo device.description=\"OBS Mic\"
    pacmd update-sink-proplist alsa_output.platform-snd_aloop.0.analog-stereo device.description=\"OBS Speaker\"

    # Run an ffmpeg rtmp server that outputs to /dev/video10
    
}

pico() {
    sudo picocom -b 115200 -r -l /dev/ttyUSB0
}

uart() {
    picocom -b 115200 -r -l /dev/ttyUSB0
}

rdp() {
    remmina
}

