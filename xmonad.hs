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
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import System.IO
import Graphics.X11.ExtraTypes.XF86

myTerm = "terminator"

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myMetaKey = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll $ [
	title =? "gpaste-zenity" --> doCenterFloat
	]

myConfig = defaultConfig {
	manageHook  = manageDocks <+> manageHook defaultConfig <+> myManageHook,
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
	avoidStruts (
		(tiled)
		||| (Mirror tiled)
		||| (Grid)
		||| (Circle)
		||| (simpleCross)
		||| (spiral (6/7))
		||| (ThreeColMid 1 delta (1/2))
		||| (tabbed shrinkText myTabTheme)
	)
	||| simplestFloat
	||| noBorders Full
		where
			tiled = Tall 1 delta ration
			ration = 2/3 -- master proportion
			delta = 3/100 -- percent of master resize

cmdStdSuffix = " &>/dev/null"

cmd   (cmdStr) = cmdStr ++ cmdStdSuffix
cmdKb (layout) = cmd ("xkb-switch -s " ++ layout)
cmdAudioSetVol (vol) = "pactl set-sink-volume 0 " ++ vol

cmdAudioMute     = cmd "pactl set-sink-mute 0 true"
cmdAudioUnmute   = cmd "pactl set-sink-mute 0 false"
cmdAudioToggle   = cmd "pactl set-sink-mute 0 toggle"
cmdAudioInc      = cmd (cmdAudioUnmute ++ ";" ++ (cmdAudioSetVol "+1.0dB"))
cmdAudioDec      = cmd (cmdAudioUnmute ++ ";" ++ (cmdAudioSetVol "-1.0dB"))

cmdScrnShot      = cmd "gnome-screenshot"
cmdScrnShotArea  = cmd "gnome-screenshot -a"
cmdScrnShotX     = cmd "gnome-screenshot -i"
cmdScrnShotAreaX = cmd "gnome-screenshot -ia"

myKeys = [
	
	-- required https://github.com/ierton/xkb-switch
	((myMetaKey, xK_z), spawn (cmdKb "us")),
	((myMetaKey, xK_x), spawn (cmdKb "ru")),
	
	-- required https://github.com/unclechu/gpaste-zenity
	((myMetaKey, xK_v), spawn (cmd "gpaste-zenity.sh")),
	
	-- screenshots (basic keyboard)
	
	((0, xK_Print),         spawn cmdScrnShot),
	((myMetaKey, xK_Print), spawn cmdScrnShotArea),
	
	-- screenshots (apple keyboard)
	
	-- save to file
	((0, xF86XK_Launch5),         spawn cmdScrnShot),
	((myMetaKey, xF86XK_Launch5), spawn cmdScrnShotArea),
	-- interactive options
	((0, xF86XK_Launch6),         spawn cmdScrnShotX),
	((myMetaKey, xF86XK_Launch6), spawn cmdScrnShotAreaX),
	
	-- pulseaudio volume control
	
	((0, xF86XK_AudioMute),        spawn cmdAudioToggle),
	((0, xF86XK_AudioLowerVolume), spawn cmdAudioDec),
	((0, xF86XK_AudioRaiseVolume), spawn cmdAudioInc),
	
	-- audacious playback
	
	((myMetaKey, xF86XK_AudioPlay), spawn (cmd "audacious --play")),
	((0, xF86XK_AudioPlay), spawn (cmd "pidof audacious && audacious --pause || audacious --play")),
	((0, xF86XK_AudioPrev), spawn (cmd "audacious --rew")),
	((0, xF86XK_AudioNext), spawn (cmd "audacious --fwd"))
	
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
