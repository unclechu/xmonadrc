import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Cross
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

import System.IO

myTerm = "terminator"

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myMetaKey = mod4Mask

myConfig = defaultConfig {
	manageHook  = manageDocks <+> manageHook defaultConfig,
	layoutHook  = myLayoutHook,
	
	borderWidth = 1,
	
	modMask     = myMetaKey,
	terminal    = myTerm,
	workspaces  = myWorkspaces
}

myTabTheme = defaultTheme {
	activeColor         = "#3c5863"   ,
	activeBorderColor   = "#000000"   ,
	inactiveColor       = "#666666"   ,
	inactiveBorderColor = "#000000"   ,
	activeTextColor     = "lightgray" ,
	inactiveTextColor   = "#aaa"      ,
	decoHeight          = 12          ,
	fontName            = "terminus"
}

myLayoutHook =
	avoidStruts (tiled)
	||| avoidStruts (Mirror tiled)
	||| avoidStruts (Grid)
	||| avoidStruts (Circle)
	||| avoidStruts (simpleCross)
	||| avoidStruts (spiral (6/7))
	||| avoidStruts (ThreeCol 1 delta (1/2))
	||| avoidStruts (ThreeColMid 1 delta (1/2))
	||| avoidStruts (tabbed shrinkText myTabTheme)
	||| simplestFloat
	||| noBorders Full
		where
			tiled = Tall 1 delta ration
			ration = 2/3 -- master proportion
			delta = 3/100 -- percent of master resize


-- required this app: https://github.com/ierton/xkb-switch
myKeys = [
	((myMetaKey, xK_z), spawn "xkb-switch -s us &>/dev/null"),
	((myMetaKey, xK_x), spawn "xkb-switch -s ru &>/dev/null")
	]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
	xmonad $ myConfig {
		logHook = do
			dynamicLogWithPP $ defaultPP {
				ppOutput = System.IO.hPutStrLn xmproc,
				ppTitle = xmobarColor "gray" "" .wrap " <fc=#FFB6B0>[</fc> " "",
				ppCurrent = xmobarColor "green" "" . wrap "[" "]",
				ppSep = "  ",
				ppWsSep = " ",
				--ppLayout = const ""
				ppLayout  = (\ x -> case x of
					"Tall"            -> "[>]"
					"Mirror Tall"     -> "[v]"
					"Grid"            -> "[+]"
					"Circle"          -> "[o]"
					"Cross"           -> "[x]"
					"Spiral"          -> "[0]"
					"ThreeCol"        -> "[3]"
					"Tabbed Simplest" -> "[t]"
					"SimplestFloat"   -> "[f]"
					"Full"            -> "[ ]"
					_                 ->   x  ),
				ppHiddenNoWindows = showNamedWorkspaces
			}
			fadeInactiveLogHook 0.9
	} `additionalKeys` myKeys
		where showNamedWorkspaces wsId = wsId
