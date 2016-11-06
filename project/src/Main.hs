-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PatternGuards #-}

module Main (main) where

import qualified XMonad as XM
import XMonad ( (=?), (-->), (<&&>), (<+>), (|||), (.|.)

              , Mirror(Mirror)
              , Full(Full)
              , Tall(Tall)

              , windows
              , spawn
              , kill
              , sendMessage
              , setLayout
              , asks

              , shiftMask
              , controlMask
              , mod1Mask

              )
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

import XMonad.Layout.Grid (Grid(Grid))
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Cross (simpleCross)
import XMonad.Layout.Circle (Circle(Circle))
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))

import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import XMonad.Actions.CycleWS (prevWS, nextWS, shiftToPrev, shiftToNext)

import qualified Graphics.X11.ExtraTypes.XF86 as XF86

import System.IO (hPutStrLn)
import System.Exit (exitSuccess)

import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.List as L

import Utils (doRepeat, xmobarEscape)
import Utils.CustomConfig (getCustomConfig, Config(..))


myWorkspacesBareList :: [String]
myWorkspacesBareList  = map show [1..8]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ myWorkspacesBareList
  where
    clickable l = [ "<action=xdotool key super+" ++ k ++ ">" ++ ws ++ "</action>"
                  | (k, ws) <- zip myWorkspacesKeysList l ]
    myWorkspacesKeysList = map numpadHackMap [1..8]
    numpadHackMap x = case x of
                           0 -> "KP_Insert"
                           1 -> "KP_End"
                           2 -> "KP_Down"
                           3 -> "KP_Next"
                           4 -> "KP_Left"
                           5 -> "KP_Begin"
                           6 -> "KP_Right"
                           7 -> "KP_Home"
                           8 -> "KP_Up"
                           9 -> "KP_Prior"

myManageHook :: XM.ManageHook
myManageHook = XM.composeAll $

  [ XM.className =? "Gmrun"                     --> doCenterFloat

  , XM.title     =? "gpaste-zenity"             --> doCenterFloat

  -- gimp
  , wmRole    =? "gimp-toolbox-color-dialog" --> doCenterFloat
  , wmRole    =? "gimp-message-dialog"       --> doCenterFloat
  , wmRole    =? "gimp-layer-new"            --> doCenterFloat
  , wmRole    =? "gimp-image-new"            --> doCenterFloat

  , XM.className =? "qjackctl"                  --> doCenterFloat
  , XM.className =? "Audacious"                 --> moveTo (last $
                                                            init myWorkspaces)

  , XM.className =? "Gajim"                     --> moveTo (last myWorkspaces)
  , XM.className =? "Hexchat"                   --> moveTo (last myWorkspaces)
  , XM.className =? "utox"                      --> moveTo (last myWorkspaces)
  , XM.className =? "qTox"                      --> moveTo (last myWorkspaces)
  , XM.className =? "Gnome-ring"                --> moveTo (last myWorkspaces)

  , XM.className =? "Firefox"                   --> moveTo (head myWorkspaces)
  ]
  -- audacious
  ++ [ XM.className =? "Audacious" <&&> XM.title =? x --> doCenterFloat
     | x <- [ "Song Info"
            , "Audacious Settings"
            , "JACK Output Settings"
            , "Add Files"
            , "Open Files"
            ]
     ]
    where wmRole = XM.stringProperty "WM_WINDOW_ROLE"
          moveTo = XM.doF . W.shift

myConfig customConfig = XM.defaultConfig
  { XM.manageHook        = manageDocks <+> myManageHook
  , XM.layoutHook        = myLayoutHook

  , XM.borderWidth       = 1

  , XM.modMask           = cfgMetaKey customConfig
  , XM.terminal          = cfgTerminal customConfig
  , XM.workspaces        = myWorkspaces

  , XM.focusFollowsMouse = False
  , XM.clickJustFocuses  = True
  }
  where
    myLayoutHook =

      onWorkspace (last myWorkspaces)        lastWorkspacesLayouts $
      onWorkspace (last $ init myWorkspaces) lastWorkspacesLayouts $

      onWorkspace (myWorkspaces !! 2) -- 3th ws
                  ((avoidStruts $ simpleCross
                               ||| Circle
                               ||| centerMaster Grid
                               ||| tabbedLayout
                               ||| tiled
                               ||| Mirror tiled
                               ||| Grid
                               ||| mySpiral)
                  ||| simplestFloat
                  ||| noBorders Full) $

      (avoidStruts $  tiled
                  ||| Mirror tiled
                  ||| Grid
                  ||| mySpiral
                  ||| simpleCross
                  ||| Circle
                  ||| centerMaster Grid
                  ||| tabbedLayout
                  ||| ThreeColMid 1 delta (1/2))
      ||| simplestFloat
      ||| noBorders Full

        where
          tiled        = Tall 1 delta ration
          ration       = 2/3 -- master proportion
          delta        = 3/100 -- percent of master resize
          tabbedLayout = Tabbed.tabbed Tabbed.shrinkText myTabTheme
          mySpiral     = spiral (6/7)

          lastWorkspacesLayouts = avoidStruts
                                $  simpleCross
                               ||| Circle
                               ||| centerMaster Grid
                               ||| tabbedLayout

myTabTheme = Tabbed.defaultTheme
  { Tabbed.activeColor         = "#3c5863"
  , Tabbed.activeBorderColor   = "#000000"
  , Tabbed.inactiveColor       = "#666666"
  , Tabbed.inactiveBorderColor = "#000000"
  , Tabbed.activeTextColor     = "lightgray"
  , Tabbed.inactiveTextColor   = "#aaa"
  , Tabbed.decoHeight          = 12
  , Tabbed.fontName            = "terminus"
  }

myKeys customConfig =
  let jumpOverVisibleNext = windows $ jumpOverVisibleView (+1)
      jumpOverVisiblePrev = windows $ jumpOverVisibleView (subtract 1)
      jumpOverVisibleView affect s =
        W.greedyView (myWorkspaces !! getIdx getCurIdx) s
        where getCurIdx :: Int
              getCurIdx = fromJust $ L.elemIndex (W.currentTag s) myWorkspaces
              getIdx fromIdx
                | isHidden x = x
                | otherwise = getIdx x
                where x = getNextIdx fromIdx
                      isHidden i = (myWorkspaces !! i) `elem` hiddenTags
                      hiddenTags = map W.tag $ W.hidden s
              getNextIdx srcIdx
                  | next < 0 = maxIdx
                  | next > maxIdx = 0
                  | otherwise = next
                  where next   = affect srcIdx
                        maxIdx = subtract 1 $ length myWorkspaces
  in
  [ ((myMetaKey, XM.xK_BackSpace), spawn (cmd "autostart.sh"))


  -- required https://github.com/unclechu/gpaste-zenity
  , ((myMetaKey,              XM.xK_b),          spawn (cmd "gpaste-zenity.sh"))
  , ((myMetaKey,              XM.xK_apostrophe), spawn (cmd "gpaste-zenity.sh"))
  , ((myMetaKey .|. mod1Mask, XM.xK_b),          spawn (cmd "gpaste-zenity.sh -m=delete"))
  , ((myMetaKey .|. mod1Mask, XM.xK_apostrophe), spawn (cmd "gpaste-zenity.sh -m=delete"))


  -- screenshots (basic keyboard)

  , ((0,         XM.xK_Print), spawn cmdScrnShot)
  , ((myMetaKey, XM.xK_Print), spawn cmdScrnShotArea)
  , ((0,         XM.xK_Pause), spawn cmdScrnShotX)
  , ((myMetaKey, XM.xK_Pause), spawn cmdScrnShotAreaX)


  -- pulseaudio volume control

  , ((0, XF86.xF86XK_AudioMute),        spawn cmdAudioMute)
  , ((0, XF86.xF86XK_AudioLowerVolume), spawn cmdAudioDec)
  , ((0, XF86.xF86XK_AudioRaiseVolume), spawn cmdAudioInc)


  -- audacious playback

  , ((myMetaKey, XF86.xF86XK_AudioPlay), spawn (cmd "audacious --play"))
  , ((0,         XF86.xF86XK_AudioPlay), spawn (cmd "audacious --play-pause"))
  , ((0,         XF86.xF86XK_AudioPrev), spawn (cmd "audacious --rew"))
  , ((0,         XF86.xF86XK_AudioNext), spawn (cmd "audacious --fwd"))
  , ((0,         XF86.xF86XK_AudioStop), spawn (cmd "audacious --stop"))

  -- calculator

  , ((0, XF86.xF86XK_Calculator), spawn (cmd "gnome-calculator"))


  , ((myMetaKey, XM.xK_p),            spawn (cmd $ cfgLauncher      customConfig))

  , ((myMetaKey, XM.xK_d),            spawn (cmd $ cfgTerminalDark  customConfig))
  , ((myMetaKey, XM.xK_bracketleft),  spawn (cmd $ cfgTerminalDark  customConfig))
  , ((myMetaKey, XM.xK_s),            spawn (cmd $ cfgTerminalLight customConfig))
  , ((myMetaKey, XM.xK_bracketright), spawn (cmd $ cfgTerminalLight customConfig))

  , ((myMetaKey, XM.xK_f),            spawn (cmd $ cfgFileManager   customConfig))
  , ((myMetaKey, XM.xK_backslash),    spawn (cmd $ cfgFileManager   customConfig))



  -- close focused window with optional shift modifier
  , ((myMetaKey, XM.xK_slash), kill)

  , ((myMetaKey .|. shiftMask, XM.xK_grave), XM.io exitSuccess)
  -- temporay not available
  -- , ((myMetaKey, XM.xK_grave), spawn  $ "if type xmonad; then xmonad --recompile"
  --                                 ++ " && xmonad --restart;"
  --                                 ++ " else xmessage xmonad not in"
  --                                 ++ " \\$PATH: \"$PATH\"; fi")

  , ((myMetaKey,                 XM.xK_space), sendMessage XM.NextLayout)
  , ((myMetaKey .|. controlMask, XM.xK_space), doRepeat 2 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. shiftMask,   XM.xK_space), doRepeat 3 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. mod1Mask,    XM.xK_space), asks XM.config >>= setLayout . XM.layoutHook)

  -- because enter taken for right control
  -- and triggering real enter doesn't make it work
  , ((myMetaKey .|. mod1Mask,  XM.xK_m), windows W.swapMaster)
  , ((myMetaKey .|. shiftMask, XM.xK_m), windows W.swapMaster)

  , ((myMetaKey .|. mod1Mask,  XM.xK_j),       windows W.swapDown)
  , ((myMetaKey .|. shiftMask, XM.xK_j),       windows W.swapDown)
  , ((myMetaKey .|. mod1Mask,  XM.xK_k),       windows W.swapUp)
  , ((myMetaKey .|. shiftMask, XM.xK_k),       windows W.swapUp)
  , ((myMetaKey .|. mod1Mask,  XM.xK_Down),    windows W.swapDown)
  , ((myMetaKey .|. shiftMask, XM.xK_Down),    windows W.swapDown)
  , ((myMetaKey .|. mod1Mask,  XM.xK_Up),      windows W.swapUp)
  , ((myMetaKey .|. shiftMask, XM.xK_Up),      windows W.swapUp)
  , ((myMetaKey, XM.xK_Down),                  windows W.focusDown)
  , ((myMetaKey, XM.xK_Up),                    windows W.focusUp)

  , ((myMetaKey, XM.xK_Left),                  jumpOverVisiblePrev)
  , ((myMetaKey, XM.xK_Right),                 jumpOverVisibleNext)
  , ((myMetaKey .|. mod1Mask,  XM.xK_Left),    prevWS)
  , ((myMetaKey .|. mod1Mask,  XM.xK_Right),   nextWS)

  -- move windows
  , ((myMetaKey .|. shiftMask,   XM.xK_Left),  shiftToPrev)
  , ((myMetaKey .|. shiftMask,   XM.xK_Right), shiftToNext)

  , ((myMetaKey .|. controlMask, XM.xK_Up),    sendMessage (XM.IncMasterN 1))
  , ((myMetaKey .|. controlMask, XM.xK_Down),  sendMessage (XM.IncMasterN (-1)))
  , ((myMetaKey .|. controlMask, XM.xK_Left),  sendMessage XM.Shrink)
  , ((myMetaKey .|. controlMask, XM.xK_Right), sendMessage XM.Expand)
  ]

  ++

  -- move between displays by x,c,v keys
  let order = map screenNum $ cfgDisplaysOrder customConfig
      screenNum :: Int -> XM.ScreenId
      screenNum x = [0..] !! (x-1)
  in
  [((m .|. myMetaKey, k), XM.screenWorkspace sc >>= flip XM.whenJust (windows . f))
        | (k, sc) <- zip [XM.xK_x, XM.xK_c, XM.xK_v] order
        , (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  -- move between workspaces
  let keys1 = [ XM.xK_u, XM.xK_i, XM.xK_o
              , XM.xK_8, XM.xK_9, XM.xK_0
              , XM.xK_minus, XM.xK_equal
              ]
      -- support https://github.com/unclechu/X11-my-custom-layouts
      keys2 = [ XM.xK_u, XM.xK_i, XM.xK_o
              , XM.xK_asterisk, XM.xK_parenleft, XM.xK_parenright
              , XM.xK_minus, XM.xK_equal
              ]

      keys3 = [ XM.xK_q, XM.xK_w, XM.xK_e
              , XM.xK_1, XM.xK_2, XM.xK_3
              , XM.xK_4, XM.xK_5
              ]
      -- support https://github.com/unclechu/X11-my-custom-layouts
      keys4 = [ XM.xK_q, XM.xK_w, XM.xK_e
              , XM.xK_exclam, XM.xK_at, XM.xK_numbersign
              , XM.xK_dollar, XM.xK_percent
              ]

      keys5 = map numpadHackMap [ 1..8 ]
        where numpadHackMap x =
                case x of
                     0 -> XM.xK_KP_Insert
                     1 -> XM.xK_KP_End
                     2 -> XM.xK_KP_Down
                     3 -> XM.xK_KP_Next
                     4 -> XM.xK_KP_Left
                     5 -> XM.xK_KP_Begin
                     6 -> XM.xK_KP_Right
                     7 -> XM.xK_KP_Home
                     8 -> XM.xK_KP_Up
                     9 -> XM.xK_KP_Prior
      bind keys =
        [((m .|. myMetaKey, k), windows $ f i)
              | (i, k) <- zip myWorkspaces keys
              , (f, m) <- [ (myView, 0)
                          , (W.greedyView, mod1Mask)
                          , (W.shift, shiftMask) ]]

      -- switch to workspace only if it's hidden (not visible on any screen)
      myView :: (Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
      myView i s
        | Just x <- L.find ((i==) . W.tag) (W.hidden s)
        = s { W.current = (W.current s) { W.workspace = x }
            , W.hidden  = W.workspace (W.current s)
                        : L.deleteBy (equating W.tag) x (W.hidden s) }
        | otherwise = s
        where equating f x y = f x == f y

  in ( bind keys1
    ++ bind keys2
    ++ bind keys3
    ++ bind keys4
    ++ bind keys5 )

  ++

  -- do nothing by default workspaces keys
  [((m .|. myMetaKey, k), return ())
        | k <- [ XM.xK_6 .. XM.xK_7 ]
        , m <- [ 0, controlMask, shiftMask, mod1Mask ]]

  where
    myMetaKey = cfgMetaKey customConfig

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

main :: IO ()
main = do

  customConfig <- getCustomConfig

  let conf = myConfig customConfig
      keys = myKeys   customConfig

  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"

  XM.xmonad $ conf
    { XM.logHook = do
        DL.dynamicLogWithPP $ DL.defaultPP
          { DL.ppOutput  = hPutStrLn xmproc
          , DL.ppTitle   = DL.xmobarColor "gray" "#444" . DL.wrap " " " "
          , DL.ppCurrent = DL.xmobarColor "green" ""    . DL.wrap "[" "]"
          , DL.ppSep     = "  "
          , DL.ppWsSep   = " "
          , DL.ppLayout  = DL.xmobarColor "yellow" "" . layoutNameHandler
          , DL.ppHiddenNoWindows = showNamedWorkspaces
          }
        fadeInactiveLogHook 0.9
    } `additionalKeys` keys
      where
        showNamedWorkspaces wsId = wsId
        layoutNameHandler :: String -> String
        layoutNameHandler x = wrap $ xmobarEscape $ case x of
          "Tall"            -> "[>]"
          "Mirror Tall"     -> "[v]"
          "Grid"            -> "[+]"
          "Spiral"          -> "[0]"
          "Tabbed Simplest" -> "[t]"
          "Cross"           -> "[x]"
          "Circle"          -> "[o]"
          "ThreeCol"        -> "[3]"
          "SimplestFloat"   -> "[f]"
          "Full"            -> "[ ]"
          _                 ->   x
          where wrap t = "<action=xdotool key super+space>" ++ t ++ "</action>"
