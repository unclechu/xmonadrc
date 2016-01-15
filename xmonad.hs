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
import System.Directory
import System.Exit
import Graphics.X11.ExtraTypes.XF86

myTerm      = "terminator"
myTermLight = myTerm ++ " --profile light"
myTermDark  = myTerm ++ " --profile dark"

launcherApp = "gmrun"
fileManager = "pcmanfm"

myWorkspaces :: [String]
myWorkspaces =  [ "u","i","o", "7","8","9","0","-","=" ]

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
cmdAudioInc      = cmd $ cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "+1.0dB"
cmdAudioDec      = cmd $ cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "-1.0dB"

cmdScrnShot      = cmd "gnome-screenshot"
cmdScrnShotArea  = cmd "gnome-screenshot -a"
cmdScrnShotX     = cmd "gnome-screenshot -i"
cmdScrnShotAreaX = cmd "gnome-screenshot -ia"

(&) = flip ($)

-- we don't need additional modifier if we use Super key
optionalAdditionalModifier x
  | x == mod4Mask = 0
  | otherwise     = controlMask

myKeys myMetaKey =
  [ ((myMetaKey, xK_BackSpace), spawn (cmd "autostart.sh"))


  -- required https://github.com/unclechu/gpaste-zenity
  , ((myMask myMetaKey, xK_b), spawn (cmd "gpaste-zenity.sh"))


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


  , ((myMetaKey,        xK_p), spawn (cmd launcherApp))
  , ((myMask myMetaKey, xK_f), spawn (cmd fileManager))
  , ((myMetaKey,        xK_d), spawn (cmd myTermDark))
  , ((myMetaKey,        xK_s), spawn (cmd myTermLight))

  , ((0, xF86XK_Calculator), spawn (cmd "gnome-calculator"))

  -- close focused window with optional shift modifier
  , ((myMask myMetaKey, xK_n), kill)

  -- rebind to 'r' because we use 'n' for kill
  , ((myMask myMetaKey, xK_r), refresh)

  , ((myMetaKey .|. controlMask, xK_q), io exitSuccess)

  , ((myMetaKey .|. controlMask, xK_space), asks config >>= setLayout . layoutHook)
  , ((myMetaKey,                 xK_space), sendMessage NextLayout)

  , ((myMetaKey .|. controlMask, xK_j), windows W.swapDown)
  , ((myMetaKey .|. controlMask, xK_k), windows W.swapUp)
  ]

  ++

  -- move between displays by x,c,v keys
  [((m .|. myMetaKey, k), screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_x, xK_c, xK_v] [0..]
        , (f, m)  <- [(W.view, 0), (W.shift, controlMask)]]

  ++

  -- move between workspaces
  [((m .|. myMetaKey, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [       xK_u, xK_i, xK_o,
                                       xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

    where myMask x = x .|. optionalAdditionalModifier myMetaKey


parseMyMetaKey x = case x of
  "Mod1" -> mod1Mask
  _      -> mod4Mask

getMyMetaKeyFileContents = do
  homeDir <- getHomeDirectory
  let filePath = homeDir ++ "/.xmonad/myMetaKey"
  fileExists <- doesFileExist filePath

  if fileExists
     then do
          handle   <- openFile filePath ReadMode
          contents <- hGetContents handle
          let fileContents = contents & filter (\x -> x /= '\r' && x /= '\n')
          return fileContents
     else return "Mod4"

main = do
  myMetaKeyFileContents <- getMyMetaKeyFileContents

  let myMetaKey = parseMyMetaKey myMetaKeyFileContents
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
    } `additionalKeys` keys
      where showNamedWorkspaces wsId = wsId
