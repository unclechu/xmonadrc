#!/usr/bin/env bash

logfile=$(mktemp --suffix=-xmonad-unclechu-log)

while true; do

	# 'xmonad-unclechu' must be in $PATH
	# (installed to ~/.local/bin by stack).
	xmonad-unclechu 2>&1 >> "$logfile"

	(( $? != 200 )) && break # exit with 200 status means restart
	sleep 1
done
