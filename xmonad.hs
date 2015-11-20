import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Spiral
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import System.IO
import System.Directory
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

myTerm      = "terminator"
myTermLight = myTerm ++ " --profile light"
myTermDark  = myTerm ++ " --profile dark"

launcherApp = "gmrun"
fileManager = "pcmanfm"

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myManageHook :: ManageHook
myManageHook = composeAll
  [ title =? "gpaste-zenity" --> doCenterFloat
  , title =? "File Operation Progress" --> doCenterFloat
  , title =? "Copying files" --> doCenterFloat
  , title =? "Compress" --> doCenterFloat
  ]

myConfig myMetaKey = defaultConfig
  { manageHook  = manageDocks <+> manageHook defaultConfig <+> myManageHook
  , layoutHook  = myLayoutHook

  , borderWidth = 1

  , modMask     = myMetaKey
  , terminal    = myTerm
  , workspaces  = myWorkspaces
  }

myTabTheme = defaultTheme
  { activeColor         = "#3c5863"
  , activeBorderColor   = "#000000"
  , inactiveColor       = "#666666"
  , inactiveBorderColor = "#000000"
  , activeTextColor     = "lightgray"
  , inactiveTextColor   = "#aaa"
  , decoHeight          = 12
  , fontName            = "terminus"
  }

myLayoutHook =
  avoidStruts (
    tiled
    ||| Mirror tiled
    ||| Grid
    ||| spiral (6/7)
    ||| tabbed shrinkText myTabTheme
    )
  ||| simplestFloat
  ||| noBorders Full
    where
      tiled = Tall 1 delta ration
      ration = 2/3 -- master proportion
      delta = 3/100 -- percent of master resize

cmd = (++ " &>/dev/null")

cmdActiveSink =
  "\"`(pactl info"
    ++ "| grep -i 'default sink:'"
    ++ "| sed 's/^default sink:[ ]*//i') 2>/dev/null`\""
cmdAudioSetVol vol = "pactl set-sink-volume " ++ cmdActiveSink ++ ' ':vol

cmdAudioMute     = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " true"
cmdAudioUnmute   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " false"
cmdAudioToggle   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " toggle"
cmdAudioInc      = cmd $ cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "+1.0dB"
cmdAudioDec      = cmd $ cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "-1.0dB"

cmdScrnShot      = cmd "gnome-screenshot"
cmdScrnShotArea  = cmd "gnome-screenshot -a"
cmdScrnShotX     = cmd "gnome-screenshot -i"
cmdScrnShotAreaX = cmd "gnome-screenshot -ia"

(&) = flip ($)

myKeys myMetaKey =
  [ ((myMetaKey, xK_BackSpace), spawn (cmd "autostart.sh"))


  -- required https://github.com/unclechu/gpaste-zenity
  , ((myMetaKey .|. shiftMask, xK_v), spawn (cmd "gpaste-zenity.sh"))


  -- screenshots (basic keyboard)

  , ((0,         xK_Print), spawn cmdScrnShot)
  , ((myMetaKey, xK_Print), spawn cmdScrnShotArea)
  , ((0,         xK_Pause), spawn cmdScrnShotX)
  , ((myMetaKey, xK_Pause), spawn cmdScrnShotAreaX)


  -- pulseaudio volume control

  , ((0, xF86XK_AudioMute),        spawn cmdAudioMute)
  , ((0, xF86XK_AudioLowerVolume), spawn cmdAudioDec)
  , ((0, xF86XK_AudioRaiseVolume), spawn cmdAudioInc)


  -- audacious playback

  , ((myMetaKey, xF86XK_AudioPlay), spawn (cmd "audacious --play"))
  , ((0,         xF86XK_AudioPlay), spawn (cmd "audacious --play-pause"))
  , ((0,         xF86XK_AudioPrev), spawn (cmd "audacious --rew"))
  , ((0,         xF86XK_AudioNext), spawn (cmd "audacious --fwd"))


  , ((myMetaKey, xK_p), spawn (cmd launcherApp))
  , ((myMetaKey .|. shiftMask,   xK_f),      spawn (cmd fileManager))
  , ((myMetaKey .|. shiftMask,   xK_Return), spawn (cmd myTermLight))
  , ((myMetaKey .|. controlMask, xK_Return), spawn (cmd myTermDark))


  -- quit, or restart (because we used 'q' key move between displays
  , ((myMetaKey .|. shiftMask, xK_z), io (exitWith ExitSuccess))
  , ((myMetaKey              , xK_z), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ]

  ++

  -- move between displays by q,w,e instead of w,e,r
  [((m .|. myMetaKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


parseMyMetaKey x = case x of
  "Mod1" -> mod1Mask
  _      -> mod4Mask

main = do

  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/.xmonad/myMetaKey"

  handle   <- openFile filePath ReadMode
  contents <- hGetContents handle

  let fileContents = contents & filter (\x -> x /= '\r' && x /= '\n')
  let myMetaKey = parseMyMetaKey fileContents

  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ (myConfig myMetaKey)
    { logHook = do
        dynamicLogWithPP $ defaultPP
          { ppOutput  = System.IO.hPutStrLn xmproc
          , ppTitle   = xmobarColor "gray" "#444" . wrap " " " "
          , ppCurrent = xmobarColor "green" ""    . wrap "[" "]"
          , ppSep     = "  "
          , ppWsSep   = " "
          , ppLayout  = xmobarColor "yellow" "" .
              (\x -> case x of
                "Tall"            -> "[>]"
                "Mirror Tall"     -> "[v]"
                "Grid"            -> "[+]"
                "Spiral"          -> "[0]"
                "Tabbed Simplest" -> "[t]"
                "SimplestFloat"   -> "[f]"
                "Full"            -> "[ ]"
                _                 ->   x)
          , ppHiddenNoWindows = showNamedWorkspaces
          }
        fadeInactiveLogHook 0.9
    } `additionalKeys` (myKeys myMetaKey)
      where showNamedWorkspaces wsId = wsId
