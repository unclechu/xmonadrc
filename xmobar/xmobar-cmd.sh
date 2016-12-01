#!/bin/bash

PIPE_FILE="$HOME/.xmonad/xmobar.fifo"
PID_FILE="$HOME/.xmonad/xmobar-cmd.pid"

clean () {
	rm -f "$PIPE_FILE"
}

kill_old () {
	local old_pid=$(cat "$PID_FILE")
	if ps -p "$old_pid"; then
		kill -TERM "$old_pid"
		sleep 1
		if ps -p "$old_pid"; then
			kill -KILL "$old_pid"
			sleep 1
		fi
	fi
	rm -f "$PID_FILE"
}

# kill old process
if [ -f "$PID_FILE" ]; then
	kill_old 0<&- 1>/dev/null 2>/dev/null
fi

# store pid of current process
echo $$ > "$PID_FILE"

clean
mkfifo "$PIPE_FILE"

trap clean EXIT

# need to restart xlib-keys-hack
if [ -x "$HOME/.local/bin/kbd.sh" ]; then
	{
		exec 0<&- 1>/dev/null 2>/dev/null
		sleep 1
		"$HOME/.local/bin/kbd.sh"
	} &
fi

numlock_is=off
capslock_is=off
alternative_is=off

msg () {
	local numlock='<fc=#999>num</fc>'
	local capslock='<fc=#999>caps</fc>'
	local alternative='<fc=#999>hax</fc>'
	if [ $numlock_is == on ]; then
		numlock='<fc=#eee>num</fc>'
	fi
	if [ $capslock_is == on ]; then
		capslock='<fc=orange>CAPS</fc>'
	fi
	if [ $alternative_is == on ]; then
		alternative='<fc=yellow>HAX</fc>'
	fi
	numlock="<action=simulate-keys NumLock>$numlock</action>"
	capslock="<action=simulate-keys CapsLock>$capslock</action>"
	alternative="$alternative" # TODO both alts simulate (needs changes in 'xlib-keys-combo-simulator')
	echo "$numlock $capslock $alternative"
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
		elif [ "$line" == "alternative:on" ]; then
			alternative_is=on
		elif [ "$line" == "alternative:off" ]; then
			alternative_is=off
		fi
	fi
done
