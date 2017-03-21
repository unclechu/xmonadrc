all: xmonad xmobar xmobar-indicators-cmd
clean: clean-xmonad clean-xmobar clean-xmobar-indicators-cmd
test: test-xmobar-indicators-cmd

xmonad:
	./make.pl xmonad
clean-xmonad:
	./make.pl clean-xmonad

xmobar:
	./make.pl xmobar
clean-xmobar:
	./make.pl clean-xmobar

test-xmobar-indicators-cmd:
	./make.pl test-xmobar-indicators-cmd
xmobar-indicators-cmd:
	./make.pl xmobar-indicators-cmd
clean-xmobar-indicators-cmd:
	./make.pl clean-xmobar-indicators-cmd
