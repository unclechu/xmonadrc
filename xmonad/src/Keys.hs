-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PackageImports #-}

module Keys
  ( myKeys
  ) where

import "xmonad" XMonad ( (.|.)

                       , io
                       , windows
                       , spawn
                       , kill
                       , sendMessage
                       , setLayout
                       , asks
                       , withFocused

                       , shiftMask
                       , controlMask
                       , mod1Mask
                       )
import qualified "xmonad" XMonad as XM
import qualified "xmonad" XMonad.StackSet as W

import "xmonad-contrib" XMonad.Actions.CycleWS ( prevWS, nextWS
                                               , shiftToPrev, shiftToNext
                                               )
import "xmonad-contrib" XMonad.Hooks.ManageDocks (ToggleStruts(ToggleStruts))
import "xmonad-contrib" XMonad.Actions.NoBorders (toggleBorder)
import "xmonad-contrib" XMonad.Layout.ResizableTile
  (MirrorResize(MirrorShrink, MirrorExpand))
import qualified "xmonad-contrib" XMonad.Util.ExtensibleState as XS

import qualified "X11" Graphics.X11.ExtraTypes.XF86 as XF86

import "base" Control.Concurrent (forkIO)

import "base" Data.List (elemIndex, find, deleteBy)
import "base" Data.Maybe (fromJust)

import "base" System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))

-- local imports

import XMonad.Hooks.Focus (FocusLock(FocusLock))

import Workspaces (myWorkspacesBareList)
import Utils (doRepeat)
import Utils.IPC (IPCHandler, focusLockState)
import Utils.CustomConfig ( Config ( cfgMetaKey
                                   , cfgLauncher
                                   , cfgFileManager
                                   , cfgTerminalDark
                                   , cfgTerminalLight
                                   , cfgDisplaysOrder
                                   )
                          )


type KeyCombo = (XM.ButtonMask, XM.KeySym)
type KeyHook  = (KeyCombo, XM.X ())

myKeys :: IPCHandler -> [String] -> Config -> [KeyHook]
myKeys ipc myWorkspaces customConfig =
  let jumpOverVisibleNext = windows $ jumpOverVisibleView (+1)
      jumpOverVisiblePrev = windows $ jumpOverVisibleView (subtract 1)
      jumpOverVisibleView affect s =
        W.greedyView (myWorkspaces !! getIdx getCurIdx) s
        where getCurIdx :: Int
              getCurIdx = fromJust $ elemIndex (W.currentTag s) myWorkspaces
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
      incMaster = XM.IncMasterN   1
      decMaster = XM.IncMasterN (-1)
  in
  [ ((myMetaKey, XM.xK_BackSpace), spawn (cmd "autostart.sh"))


  -- required https://github.com/unclechu/gpaste-zenity
  , ((myMetaKey,              XM.xK_apostrophe), spawn (cmd "gpaste-zenity.sh"))
  , ((myMetaKey .|. mod1Mask, XM.xK_apostrophe), spawn (cmd "gpaste-zenity.sh -m=choose"))


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

  , ((myMetaKey, XM.xK_bracketleft),  spawn (cmd $ cfgTerminalDark  customConfig))
  , ((myMetaKey, XM.xK_bracketright), spawn (cmd $ cfgTerminalLight customConfig))

  , ((myMetaKey, XM.xK_backslash),    spawn (cmd $ cfgFileManager   customConfig))



  -- close focused window with optional shift modifier
  , ((myMetaKey, XM.xK_slash), kill)

  -- exit and restart (200 status means restart)
  , ((myMetaKey .|. shiftMask, XM.xK_grave), XM.io exitSuccess)
  , ((myMetaKey,               XM.xK_grave), XM.io $ exitWith $ ExitFailure 200)

  -- layouts switching
  , ((myMetaKey,                 XM.xK_space), sendMessage XM.NextLayout)
  , ((myMetaKey .|. controlMask, XM.xK_space), doRepeat 2 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. shiftMask,   XM.xK_space), doRepeat 3 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. mod1Mask,    XM.xK_space), asks XM.config >>= setLayout . XM.layoutHook)

  , ((myMetaKey, XM.xK_z), sendMessage ToggleStruts)
  , ((myMetaKey, XM.xK_a), withFocused toggleBorder)
  , ((myMetaKey, XM.xK_y), myToggleLock)

  -- because enter taken for right control
  -- and triggering real enter doesn't make it work
  , ((myMetaKey .|. mod1Mask,  XM.xK_m),       windows W.swapMaster)
  , ((myMetaKey .|. shiftMask, XM.xK_m),       windows W.swapMaster)

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

  , ((myMetaKey .|. controlMask, XM.xK_Up),    sendMessage incMaster)
  , ((myMetaKey .|. controlMask, XM.xK_Down),  sendMessage decMaster)
  , ((myMetaKey .|. controlMask, XM.xK_Left),  sendMessage XM.Shrink)
  , ((myMetaKey .|. controlMask, XM.xK_Right), sendMessage XM.Expand)

  , ((myMetaKey .|. controlMask, XM.xK_h),      doRepeat 2 $ sendMessage XM.Shrink)
  , ((myMetaKey .|. controlMask, XM.xK_l),      doRepeat 2 $ sendMessage XM.Expand)
  , ((myMetaKey .|. shiftMask,   XM.xK_h),      doRepeat 3 $ sendMessage XM.Shrink)
  , ((myMetaKey .|. shiftMask,   XM.xK_l),      doRepeat 3 $ sendMessage XM.Expand)

  , ((myMetaKey .|. controlMask, XM.xK_comma),  doRepeat 2 $ sendMessage incMaster)
  , ((myMetaKey .|. controlMask, XM.xK_period), doRepeat 2 $ sendMessage decMaster)
  , ((myMetaKey .|. shiftMask,   XM.xK_comma),  doRepeat 3 $ sendMessage incMaster)
  , ((myMetaKey .|. shiftMask,   XM.xK_period), doRepeat 3 $ sendMessage decMaster)

  , ((myMetaKey,                 XM.xK_f), sendMessage MirrorShrink) -- inc
  , ((myMetaKey,                 XM.xK_d), sendMessage MirrorExpand) -- dec
  , ((myMetaKey .|. controlMask, XM.xK_f), doRepeat 2 $ sendMessage MirrorShrink)
  , ((myMetaKey .|. controlMask, XM.xK_d), doRepeat 2 $ sendMessage MirrorExpand)
  , ((myMetaKey .|. shiftMask,   XM.xK_f), doRepeat 3 $ sendMessage MirrorShrink)
  , ((myMetaKey .|. shiftMask,   XM.xK_d), doRepeat 3 $ sendMessage MirrorExpand)
  ]

  ++

  -- move between displays by x,c,v,b keys
  let order = map screenNum $ cfgDisplaysOrder customConfig
      screenNum :: Int -> XM.ScreenId
      screenNum x = [0..] !! (x-1)

      -- see https://gist.github.com/unclechu/cba127f844a1816439fa18b77e0697f1
      cursorToDisplay position n =
        spawn $ cmd $ "cursor-to-display.sh -p " ++ position ++ " " ++ show n

      cursorToDisplayCmd n m
        | m == 0                        = cursorToDisplay "rb" n
        | m == mod1Mask                 = cursorToDisplay "lt" n
        | m == controlMask              = cursorToDisplay "cc" n
        | m == mod1Mask .|. controlMask = cursorToDisplay "rt" n
        | m == mod1Mask .|. shiftMask   = cursorToDisplay "lb" n
        | otherwise                     = return ()

   in [((m .|. myMetaKey, k), XM.screenWorkspace sc
                               >>= flip XM.whenJust (windows . f)
                                >> cursorToDisplayCmd n m)
            | (k, sc, n) <- zip3 [XM.xK_x, XM.xK_c, XM.xK_v, XM.xK_b]
                                 order ([1..] :: [Int])
            , (f, m)     <- [ (W.view,  0)
                            , (W.view,  mod1Mask)
                            , (W.view,  controlMask)
                            , (W.view,  mod1Mask .|. controlMask)
                            , (W.view,  mod1Mask .|. shiftMask)
                            , (W.shift, shiftMask)
                            ]
      ]

  ++

  -- move between workspaces
  let keysLists :: [[XM.KeySym]]
      keysLists = [ -- right hand
                    [ XM.xK_u, XM.xK_i, XM.xK_o
                    , XM.xK_7, XM.xK_8, XM.xK_9
                    , XM.xK_0, XM.xK_minus, XM.xK_equal
                    ]
                    -- left hand
                  , [ XM.xK_q, XM.xK_w, XM.xK_e
                    , XM.xK_1, XM.xK_2, XM.xK_3
                    , XM.xK_4, XM.xK_5, XM.xK_6
                    ]
                    -- numpadKeys
                  , [ case x of
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
                           _ -> error "unexpected value"
                    | x <- [1..length myWorkspacesBareList]
                    ]
                  ]

      -- helper to map this keys
      bind :: [XM.KeySym] -> [((XM.KeyMask, XM.KeySym), XM.X ())]
      bind keys =
        [((m .|. myMetaKey, k), windows $ f i)
              | (i, k) <- zip myWorkspaces keys
              , (f, m) <- [ (myView, 0)
                          , (W.greedyView, mod1Mask)
                          , (W.shift, shiftMask)
                          ]
        ]

      -- switch to workspace only if it's hidden (not visible on any screen)
      myView :: (Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
      myView i s
        | Just x <- find ((i==) . W.tag) (W.hidden s)
        = s { W.current = (W.current s) { W.workspace = x }
            , W.hidden  = W.workspace (W.current s)
                        : deleteBy (equating W.tag) x (W.hidden s)
            }
        | otherwise = s
        where equating f x y = f x == f y

  in foldr ((++) . bind) [] keysLists

  where
    myMetaKey = cfgMetaKey customConfig

    cmd = (++ " 0</dev/null 1>/dev/null 2>/dev/null")

    cmdActiveSink =
      "\"`(pactl info"
        ++ "| grep -i 'default sink:'"
        ++ "| sed 's/^default sink:[ ]*//i') 2>/dev/null`\""
    cmdAudioSetVol vol = "pactl set-sink-volume " ++ cmdActiveSink ++ ' ':vol

    cmdAudioMute     = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " true"
    cmdAudioUnmute   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " false"
    -- cmdAudioToggle   = cmd $ "pactl set-sink-mute " ++ cmdActiveSink ++ " toggle"
    cmdAudioInc      = cmd $  cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "+1.0dB"
    cmdAudioDec      = cmd $  cmdAudioUnmute ++ ";" ++ cmdAudioSetVol "-1.0dB"

    cmdScrnShot      = cmd "gnome-screenshot"
    cmdScrnShotArea  = cmd "gnome-screenshot -a"
    cmdScrnShotX     = cmd "gnome-screenshot -i"
    cmdScrnShotAreaX = cmd "gnome-screenshot -ia"

    myToggleLock :: XM.X ()
    myToggleLock = do
      FocusLock b <- XS.get
      let newValue = not b
      _ <- io $ forkIO $ focusLockState ipc newValue
      XS.put $ FocusLock newValue
