import XMonad

main = xmonad defaultConfig
	{
		modMask = mod4Mask, -- mod4 instead of alt key
		terminal = "terminator"
	}
