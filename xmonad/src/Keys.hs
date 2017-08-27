-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Keys
  ( myKeys
  , myEZKeys
  ) where

import "xmonad" XMonad ( (.|.)
                       , X

                       , io
                       , runQuery
                       , whenJust
                       , windows
                       , spawn
                       , kill
                       , sendMessage
                       , setLayout
                       , refresh
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
import "xmonad-contrib" XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts))
import "xmonad-contrib" XMonad.Hooks.ManageHelpers (pid)
import "xmonad-contrib" XMonad.Actions.NoBorders (toggleBorder)
import "xmonad-contrib" XMonad.Layout.ResizableTile
  (MirrorResize (MirrorShrink, MirrorExpand))
import qualified "xmonad-contrib" XMonad.Util.ExtensibleState as XS

import qualified "X11" Graphics.X11.ExtraTypes.XF86 as XF86

import "base" Data.List (elemIndex, find, deleteBy)
import "base" Data.Maybe (fromJust)
import "base" Data.Functor (void)

import "base" Control.Arrow ((***))
import "base" Control.Monad ((>=>), when)
import "base" Control.Concurrent (threadDelay)

import "base" System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import "unix" System.Posix.Signals (signalProcess, sigKILL)

-- local imports

import XMonad.Hooks.Focus (FocusLock (FocusLock))

import Workspaces (myWorkspacesBareList)
import Utils (doRepeat)
import Utils.IPC (IPCHandler, focusLockState, invertWindowColors)
import Utils.CustomConfig ( Config ( cfgMetaKey
                                   , cfgLauncher
                                   , cfgFileManager
                                   , cfgTerminalDark
                                   , cfgTerminalLight
                                   , cfgDisplaysOrder
                                   )
                          )


type KeyCombo = (XM.ButtonMask, XM.KeySym)
type KeyHook  = (KeyCombo, X ())


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
  , ((myMetaKey,               XM.xK_slash), kill)
  , ((myMetaKey .|. shiftMask, XM.xK_slash), exterminate)

  -- exit and restart (200 status means restart)
  , ((myMetaKey .|. shiftMask, XM.xK_grave), XM.io exitSuccess)
  , ((myMetaKey,               XM.xK_grave), XM.io $ exitWith $ ExitFailure 200)

  -- layouts switching
  , ((myMetaKey,                 XM.xK_space), sendMessage XM.NextLayout)
  , ((myMetaKey .|. controlMask, XM.xK_space), doRepeat 2 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. shiftMask,   XM.xK_space), doRepeat 3 $ sendMessage XM.NextLayout)
  , ((myMetaKey .|. mod1Mask,    XM.xK_space), asks XM.config >>= setLayout . XM.layoutHook)

  , ((myMetaKey, XM.xK_n), refresh)
  , ((myMetaKey, XM.xK_r), refresh)
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

  where
    myMetaKey = cfgMetaKey customConfig

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

    myToggleLock :: X ()
    myToggleLock = do
      FocusLock b <- XS.get
      let newValue = not b
      _ <- io $ focusLockState ipc newValue
      XS.put $ FocusLock newValue

    -- Send SIGKILL to focused window
    exterminate :: X ()
    exterminate = withFocused $
      runQuery pid >=> flip whenJust (io . signalProcess sigKILL)


myEZKeys :: IPCHandler -> [String] -> Config -> [(String, X ())]
myEZKeys ipc myWorkspaces customConfig =

  -- switching between workspaces
  let keysLists :: [([String], Maybe [String])]
      keysLists =

        [ -- right hand
          -- FIXME <minus> and <equal> doesn't work
          ( ["u", "i", "o", "7", "8", "9"{-, "0", "<minus>", "<equal>"-}]
          , Just ["u", "i", "o", "7", "8", "9"]
          )

          -- left hand
        , ( ["q", "w", "e", "1", "2", "3"{-, "4", "5", "6"-}]
          , Just ["q", "w", "e", "1", "2", "3"]
          )

          -- numpadKeys
        , ( [ case x of
                   0 -> "<KP_Insert>"
                   1 -> "<KP_End>"
                   2 -> "<KP_Down>"
                   3 -> "<KP_Next>"
                   4 -> "<KP_Left>"
                   5 -> "<KP_Begin>"
                   6 -> "<KP_Right>"
                   7 -> "<KP_Home>"
                   8 -> "<KP_Up>"
                   9 -> "<KP_Prior>"
                   _ -> error "unexpected value"
            | x <- [1..length myWorkspacesBareList]
            ]
          , Nothing
          )
        ]

      bindF, bindAltF :: [String] -> [(String, X ())]
      bindF    = getBound "M-"     myWorkspaces
      bindAltF = getBound "M-g " $ drop 3 myWorkspaces

      getBound :: String -> [String] -> [String] -> [(String, X ())]
      getBound prefix workspaces keys =
        [ (prefix ++ m ++ k, windows $ f w)
        | (w, k) <- zip workspaces keys
        , (f, m) <- [ (myView,       "")
                    , (W.greedyView, "M1-" // "M-g M1-")
                    , (W.shift,      "S-"  // "M-g S-")
                    ] ++ (
                      [ (W.greedyView, "M-g M1-")
                      , (W.shift,      "M-g S-")
                      ] // []
                    )
        ]
        where a // b = if prefix == "M-" then a else b

      -- helper to map this keys
      bind :: ([String], Maybe [String]) -> [(String, X ())]
      bind ((bindF *** maybe [] bindAltF) -> uncurry (++) -> x) = x

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

  ++

  let hookKeys = ['x', 'c', 'v', 'b']                   :: [Char]
      order = cfgDisplaysOrder customConfig ; order     :: [Int]
      screenNum x = [0..] !! (x-1)          ; screenNum :: Int -> XM.ScreenId

      -- see https://github.com/unclechu/place-cursor-at
      -- see https://gist.github.com/unclechu/cba127f844a1816439fa18b77e0697f1
      handler :: Int
              -- ^ Bare screen num
              -> Int
              -- ^ Ordered screen num
              -> (XM.WorkspaceId -> XM.WindowSet -> XM.WindowSet)
              -> Bool
              -- ^ Show GUI to place cursor more precisely on specified screen
              -> X ()
      handler snBare snOrdered f precise = do

        XM.screenWorkspace (screenNum snOrdered)
          >>= flip XM.whenJust (windows . f)

        spawn $ cmd $ "cursor-to-display.sh -p rb " ++ show snBare
        when precise $ spawn $ cmd $ "place-cursor-at " ++ show snOrdered

   in [ (fk [k], handler snBare snOrdered f p)
      | (k, snOrdered, snBare) <- zip3 hookKeys order [(1 :: Int)..]
      , (p, f, fk) <- [ (False, W.view,  ("M-"     ++))
                      , (True,  W.view,  ("M-M1-"  ++))
                      , (True,  W.view,  ("M-C-"   ++))
                      , (False, W.shift, ("M-S-"   ++))
                      , (True,  W.view,  ("M-g "   ++))
                      , (False, W.shift, ("M-g S-" ++))
                      ]
      ]

  ++

  [ ("M-g i", invertColors)

  , ("M-g S-z", sendMessage ToggleStruts)
  , ("M-g z",   withFocused toggleBorder >> refresh)
  , ("M-z",     withFocused toggleBorder
                >> refresh
                >> io (threadDelay $ 500 * 1000)
                >> withFocused toggleBorder
                >> refresh)
  ]

  where invertColors :: X ()
        invertColors = withFocused $ void . io . invertWindowColors ipc


cmd :: String -> String
cmd = (++ " 0</dev/null 1>/dev/null 2>/dev/null")
