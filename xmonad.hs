import XMonad
import XMonad.Util.Run (spawnPipe)

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Cross
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import System.IO

myTerm = "terminator"

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myConfig = defaultConfig {
	manageHook  = manageDocks <+> manageHook defaultConfig,
	layoutHook  = avoidStruts $ myLayoutHook,
	
	borderWidth = 2,
	
	modMask     = mod4Mask, -- mod4 instead of alt key
	terminal    = myTerm,
	workspaces  = myWorkspaces
}

myTabTheme = defaultTheme {
	activeColor         = "#3c5863",
	activeBorderColor   = "#000000",
	inactiveColor       = "#666666",
	inactiveBorderColor = "#000000",
	decoHeight          = 10
}

myLayoutHook =
	tiled
	||| Mirror tiled
	||| Grid
	||| Circle
	||| simpleCross
	||| spiral (6/7)
	||| ThreeCol 1 delta (1/2)
	||| ThreeColMid 1 delta (1/2)
	||| tabbed shrinkText myTabTheme
	||| noBorders (fullscreenFull Full)
		where
			tiled = Tall 1 delta ration
			ration = 2/3 -- master proportion
			delta = 3/100 -- percent of master resize

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
	xmonad $ myConfig {
		logHook = dynamicLogWithPP $ defaultPP {
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
				"Full"            -> "[ ]"
				_                 ->   x   ),
			ppHiddenNoWindows = showNamedWorkspaces
		}
	} where showNamedWorkspaces wsId = wsId
