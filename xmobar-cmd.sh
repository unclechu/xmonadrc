#!/bin/bash

PIPE_FILE="$HOME/.xmonad/xmobar.fifo"

if [[ ! -p "$PIPE_FILE" ]]; then
	mkfifo "$PIPE_FILE"
fi

level3_is=off

msg () {
	local level3_color=
	if [ $level3_is == on ]; then
		level3_color=yellow
	else
		level3_color=blue
	fi
	echo "<fc=$level3_color>L3</fc>"
}

while true; do
	msg
	if read line < "$PIPE_FILE"; then
		if [ "$line" == "level3:on" ]; then
			level3_is=on
		elif [ "$line" == "level3:off" ]; then
			level3_is=off
		fi
	fi
done
