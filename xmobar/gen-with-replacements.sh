#!/usr/bin/env bash
#
# Generates xmobar config with some customizations
# taken from '~/.xmonad/xmobar.replacements.hs'
# and saves it to '~/.xmonad/xmobar.hs'.
#
# Example of '~/.xmonad/xmobar.replacements.hs':
#   {-^ position ^-}
#   position = Static { xpos = 1440, ypos = 700, width = 1080, height = 10 }
#   {-$ position $-}
#

exec 0<&-

DIR=$(dirname "$0")
SRC_FILE=$DIR/xmobar.hs
DEST_FILE=~/.xmonad/xmobar.generated.hs
REPLACEMENTS_FILE=~/.xmonad/xmobar.replacements.hs

REPLACEMENT_REG_BEGIN='^{-^ \([a-zA-Z]\+\) ^-}$'
REPLACEMENT_REG_END='^{-$ \([a-zA-Z]\+\) $-}$'

SED_TRIM='s/\(^\s\+\|\s\+$\)//g'

# Run with '--clean' to remove previously
if (( $# == 1 )) && [[ $1 == --clean ]]; then
	rm -f "$DEST_FILE"
	exit 0
elif (( $# != 0 )); then
	echo "Incorrect arguments: $@" 1>&2
	exit 1
fi

declare -A replacements


function preprocess {

	echo '-- This file was generated automatically.'
	echo '-- Please, do not edit, it could be overwritten.'
	echo '-- Use ~/.xmonad/xmobar.replacements.hs if you need to customize it.'
	echo

	# Name of current replacement
	local current_replacement=

	# To aggregate default value here that won't be replaced
	local default_value=

	local SRC_LINES=$(cat "$1")

	for i in $(seq "`echo "$SRC_LINES" | wc -l`"); do

		line=$(echo "$SRC_LINES" | head -n "$i" | tail -n 1 | sed "$SED_TRIM")

		# Ingoring lines if it's empty or just a comment
		if [[ $line == '' ]] || echo "$line" | grep '^--' 1>/dev/null; then
			continue
		fi

		if echo "$line" | grep "$REPLACEMENT_REG_BEGIN" 1>/dev/null; then

			local name=$(echo "$line" | sed "s/$REPLACEMENT_REG_BEGIN/\1/")

			if [[ $current_replacement != '' ]]; then
				echo \
					'Preprocessing error:' \
					"Found opening of '$name' replacement" \
					"but previously opened '$current_replacement'" \
					'replacement is not closed yet' 1>&2
				return 1
			fi

			current_replacement=$name

		elif echo "$line" | grep "$REPLACEMENT_REG_END" 1>/dev/null; then

			local name=$(echo "$line" | sed "s/$REPLACEMENT_REG_END/\1/")

			if [[ $current_replacement == '' ]]; then
				echo \
					'Preprocessing error:' \
					"Found closing of '$name' replacement" \
					'but it was not previously opened' 1>&2
				return 1
			elif [[ $current_replacement != $name ]]; then
				echo \
					'Preprocessing error:' \
					"Found closing of '$name' replacement" \
					"but previously opened was '$current_replacement'" 1>&2
				return 1
			fi

			for key in "${!replacements[@]}"; do
				if [[ $key == $name ]]; then
					echo -n "${replacements[$name]}"
					default_value=
					break
				fi
			done

			[[ $default_value != '' ]] && echo -n "$default_value"
			current_replacement=
			default_value=

		elif [[ $current_replacement != '' ]]; then
			default_value=${default_value}${line}$'\n'
		else
			echo "$line"
		fi

	done

	if [[ $current_replacement != '' ]]; then
		echo \
			'Preprocessing error:' \
			"It's EOF but replacement '$current_replacement' isn't closed" 1>&2
		return 1
	fi
}

function generate_and_save {

	rm -f "$DEST_FILE"
	preprocess "$SRC_FILE" > "$DEST_FILE"
	retval=$?

	if (( $retval != 0 )); then
		rm -f "$DEST_FILE"
		return "$retval"
	fi
}


if [[ ! -f $REPLACEMENTS_FILE ]]; then
	generate_and_save
	exit $?
fi


# Extracting replacements

REPLACEMENTS_LINES=$(cat "$REPLACEMENTS_FILE")

# Name of current replacement
current_replacement=

for i in $(seq "`echo "$REPLACEMENTS_LINES" | wc -l`"); do

	line=$(
		echo "$REPLACEMENTS_LINES" | head -n "$i" | tail -n 1 | sed "$SED_TRIM"
	)

	# Ingoring lines if it's empty or just a comment
	if [[ $line == '' ]] || echo "$line" | grep '^--' 1>/dev/null; then
		continue
	fi

	if echo "$line" | grep -i "$REPLACEMENT_REG_BEGIN" 1>/dev/null; then

		name=$(echo "$line" | sed "s/$REPLACEMENT_REG_BEGIN/\1/")

		if [[ $current_replacement != '' ]]; then
			echo \
				"Extracting replacement from '$REPLACEMENTS_FILE' error:" \
				"Found opening of '$name' replacement" \
				"but previously opened '$current_replacement'" \
				'replacement is not closed yet' 1>&2
			exit 1
		fi

		current_replacement=$name
		replacements[$name]=

	elif echo "$line" | grep -i "$REPLACEMENT_REG_END" 1>/dev/null; then

		name=$(echo "$line" | sed "s/$REPLACEMENT_REG_END/\1/")

		if [[ $current_replacement == '' ]]; then
			echo \
				"Extracting replacement from '$REPLACEMENTS_FILE' error:" \
				"Found closing of '$name' replacement" \
				'but it was not previously opened' 1>&2
			exit 1
		elif [[ $current_replacement != $name ]]; then
			echo \
				"Extracting replacement from '$REPLACEMENTS_FILE' error:" \
				"Found closing of '$name' replacement" \
				"but previously opened was '$current_replacement'" 1>&2
			exit 1
		fi

		current_replacement=

	elif [[ $current_replacement != '' ]]; then
		replacements[$name]=${replacements[$name]}${line}$'\n'
	fi
done

if [[ $current_replacement != '' ]]; then
	echo \
		"Extracting replacement from '$REPLACEMENTS_FILE' error:" \
		"It's EOF but replacement '$current_replacement' isn't closed" 1>&2
	exit 1
fi

generate_and_save
exit $?
