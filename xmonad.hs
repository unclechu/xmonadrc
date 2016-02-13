import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Spiral
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import System.IO
import System.Exit
import Graphics.X11.ExtraTypes.XF86

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myTerm      = "terminator"
myTermLight = myTerm ++ " --profile light"
myTermDark  = myTerm ++ " --profile dark"

launcherApp = "gmrun"
fileManager = "nautilus"

myWorkspacesBareList :: [String]
myWorkspacesBareList = [ "u","i","o", "8","9","0", "-",    "=" ]
myWorkspacesKeysList :: [String]
myWorkspacesKeysList = [ "u","i","o", "8","9","0", "minus","equal" ]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ myWorkspacesBareList
  where
    clickable l = [ "<action=xdotool key super+" ++ k ++ ">" ++ ws ++ "</action>"
                  | (k,ws) <- zip myWorkspacesKeysList l ]

myManageHook :: ManageHook
myManageHook =  composeAll
  [ title    =? "gpaste-zenity"           --> doCenterFloat
  , title    =? "File Operation Progress" --> doCenterFloat
  , title    =? "Copying files"           --> doCenterFloat
  , title    =? "Compress"                --> doCenterFloat
  ]

myConfig myMetaKey = defaultConfig
  { manageHook  = manageDocks <+> myManageHook
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
  onWorkspace (last myWorkspaces) (avoidStruts tabbedLayout) $
  avoidStruts (
    tiled
    ||| Mirror tiled
    ||| Grid
    ||| spiral (6/7)
    ||| tabbedLayout
    )
  ||| simplestFloat
  ||| noBorders Full
    where
      tiled        = Tall 1 delta ration
      ration       = 2/3 -- master proportion
      delta        = 3/100 -- percent of master resize
      tabbedLayout = tabbed shrinkText myTabTheme

cmd = (++ " &>/dev/null")

cmdActiveSink =
  "\"`(pactl info"
    ++ "| grep -i 'default sink:'"
    ++ "| sed 's/^default sink:[ ]*//i') 2>/dev/null`\""
cmdAudioSetVol vol = "pactl set-sink-volume " ++ cmdActiveSink ++ ' ':vol

cmdAudioMute     = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " true"
cmdAudioUnmute   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " false"
cmdAudioToggle   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " toggle"
cmdAudioInc      = cmd $  cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "+1.0dB"
cmdAudioDec      = cmd $  cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "-1.0dB"

cmdScrnShot      = cmd "gnome-screenshot"
cmdScrnShotArea  = cmd "gnome-screenshot -a"
cmdScrnShotX     = cmd "gnome-screenshot -i"
cmdScrnShotAreaX = cmd "gnome-screenshot -ia"

(&) = flip ($)

myKeys myMetaKey =
  [ ((myMetaKey, xK_BackSpace), spawn (cmd "autostart.sh"))


  -- required https://github.com/unclechu/gpaste-zenity
  , ((myMetaKey,              xK_b), spawn (cmd "gpaste-zenity.sh"))
  , ((myMetaKey .|. mod1Mask, xK_b), spawn (cmd "gpaste-zenity.sh -m=delete"))


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
  , ((0,         xF86XK_AudioStop), spawn (cmd "audacious --stop"))


  , ((myMetaKey, xK_p), spawn (cmd launcherApp))
  , ((myMetaKey, xK_f), spawn (cmd fileManager))
  , ((myMetaKey, xK_d), spawn (cmd myTermDark))
  , ((myMetaKey, xK_s), spawn (cmd myTermLight))

  , ((0, xF86XK_Calculator), spawn (cmd "gnome-calculator"))

  -- close focused window with optional shift modifier
  , ((myMetaKey, xK_slash), kill)

  , ((myMetaKey .|. controlMask, xK_q), io exitSuccess)

  , ((myMetaKey .|. mod1Mask, xK_space), asks config >>= setLayout . layoutHook)
  , ((myMetaKey,              xK_space), sendMessage NextLayout)

  , ((myMetaKey .|. mod1Mask, xK_j), windows W.swapDown)
  , ((myMetaKey .|. mod1Mask, xK_k), windows W.swapUp)
  ]

  ++

  -- move between displays by x,c,v keys
  [((m .|. myMetaKey, k), screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_x, xK_c, xK_v] [0..]
        , (f, m)  <- [(W.view, 0), (W.shift, mod1Mask)]]

  ++

  -- move between workspaces
  [((m .|. myMetaKey, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [ xK_u, xK_i, xK_o,
                                       xK_8, xK_9, xK_0,  xK_minus, xK_equal ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, mod1Mask)]]

  ++

  -- do nothing by default workspaces keys
  [((m .|. myMetaKey, k), return ())
        | k <- [ xK_1 .. xK_7 ]
        , m <- [ 0, controlMask, shiftMask ]]

layoutNameHandler x = wrap $ xmobarEscape $ case x of
  "Tall"            -> "[>]"
  "Mirror Tall"     -> "[v]"
  "Grid"            -> "[+]"
  "Spiral"          -> "[0]"
  "Tabbed Simplest" -> "[t]"
  "SimplestFloat"   -> "[f]"
  "Full"            -> "[ ]"
  _                 ->   x
  where wrap t = "<action=xdotool key super+space>" ++ t ++ "</action>"

main = do
  let myMetaKey = mod4Mask
  let conf      = myConfig myMetaKey
  let keys      = myKeys myMetaKey

  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ conf
    { logHook = do
        dynamicLogWithPP $ defaultPP
          { ppOutput  = System.IO.hPutStrLn xmproc
          , ppTitle   = xmobarColor "gray" "#444" . wrap " " " "
          , ppCurrent = xmobarColor "green" ""    . wrap "[" "]"
          , ppSep     = "  "
          , ppWsSep   = " "
          , ppLayout  = xmobarColor "yellow" "" . layoutNameHandler
          , ppHiddenNoWindows = showNamedWorkspaces
          }
        fadeInactiveLogHook 0.9
    } `additionalKeys` keys
      where showNamedWorkspaces wsId = wsId
