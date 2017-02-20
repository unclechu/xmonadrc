all: xmonad xmobar xmobar-indicators-cmd
clean: clean-xmonad clean-xmobar clean-xmobar-indicators-cmd
test: test-xmobar-indicators-cmd

xmonad:
	(cd xmonad && stack build --install-ghc && stack install)
clean-xmonad:
	(cd xmonad && stack clean)

xmobar:
	(cd xmobar && ./gen-with-replacements.sh)
clean-xmobar:
	(cd xmobar && ./gen-with-replacements.sh --clean)

test-xmobar-indicators-cmd:
	(cd xmobar/indicators-cmd && stack build --install-ghc && stack test)
xmobar-indicators-cmd:
	(cd xmobar/indicators-cmd && stack build --install-ghc && stack install)
clean-xmobar-indicators-cmd:
	(cd xmobar/indicators-cmd && stack clean)
