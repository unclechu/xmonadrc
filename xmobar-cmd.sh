#!/bin/bash

PIPE_FILE="$HOME/.xmonad/xmobar.fifo"

if [[ ! -p "$PIPE_FILE" ]]; then
	mkfifo "$PIPE_FILE"
fi

numlock_is=off
capslock_is=off
level3_is=off

msg () {
	local numlock_color=blue
	local capslock_color=blue
	local level3_color=blue
	if [ $numlock_is == on ]; then
		numlock_color=purple
	fi
	if [ $capslock_is == on ]; then
		capslock_color=purple
	fi
	if [ $level3_is == on ]; then
		level3_color=yellow
	fi
	echo "<fc=$numlock_color>N</fc> <fc=$capslock_color>C</fc> <fc=$level3_color>L3</fc>"
}

while true; do
	msg
	if read line < "$PIPE_FILE"; then
		if [ "$line" == "numlock:on" ]; then
			numlock_is=on
		elif [ "$line" == "numlock:off" ]; then
			numlock_is=off
		elif [ "$line" == "capslock:on" ]; then
			capslock_is=on
		elif [ "$line" == "capslock:off" ]; then
			capslock_is=off
		elif [ "$line" == "level3:on" ]; then
			level3_is=on
		elif [ "$line" == "level3:off" ]; then
			level3_is=off
		fi
	fi
done
