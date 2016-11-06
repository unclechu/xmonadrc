#!/bin/bash

xmonad="$HOME/.xmonad/xmonadrc-unclechu"

while true; do
	"$xmonad"
	# exit with 200 status means restart
	[ $? -ne 200 ] && break
	sleep 1
done
