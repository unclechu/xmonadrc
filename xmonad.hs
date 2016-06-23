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
import XMonad.Layout.Cross
import XMonad.Layout.Circle
import XMonad.Layout.CenteredMaster

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import XMonad.Actions.CycleWS (prevWS, nextWS, shiftToPrev, shiftToNext)

import System.IO
import System.Exit
import Graphics.X11.ExtraTypes.XF86

import System.Directory (getHomeDirectory)
import qualified System.IO.Error as Error
import qualified Control.Exception as Exception
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Data.Char (toLower)
import Control.Monad (liftM)
import qualified Data.List as L

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

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

myManageHook :: ManageHook
myManageHook = composeAll $

  [ className =? "Gmrun"                     --> doCenterFloat

  , title     =? "gpaste-zenity"             --> doCenterFloat

  -- gimp
  , wmRole    =? "gimp-toolbox-color-dialog" --> doCenterFloat
  , wmRole    =? "gimp-message-dialog"       --> doCenterFloat
  , wmRole    =? "gimp-layer-new"            --> doCenterFloat
  , wmRole    =? "gimp-image-new"            --> doCenterFloat

  , className =? "qjackctl"                  --> doCenterFloat
  , className =? "Audacious"                 --> moveTo (last $ init myWorkspaces)

  , className =? "Gajim"                     --> moveTo (last myWorkspaces)
  , className =? "Hexchat"                   --> moveTo (last myWorkspaces)
  , className =? "utox"                      --> moveTo (last myWorkspaces)
  , className =? "qTox"                      --> moveTo (last myWorkspaces)

  , className =? "Firefox"                   --> moveTo (head myWorkspaces)
  ]
  -- audacious
  ++ [ className =? "Audacious" <&&> title =? x --> doCenterFloat
     | x <- [ "Song Info"
            , "Audacious Settings"
            , "JACK Output Settings"
            , "Add Files"
            , "Open Files"
            ]
     ]
    where wmRole = stringProperty "WM_WINDOW_ROLE"
          moveTo = doF . W.shift

myConfig customConfig = defaultConfig
  { manageHook        = manageDocks <+> myManageHook
  , layoutHook        = myLayoutHook

  , borderWidth       = 1

  , modMask           = cfgMetaKey customConfig
  , terminal          = cfgTerminal customConfig
  , workspaces        = myWorkspaces

  , focusFollowsMouse = False
  , clickJustFocuses  = True
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
                  ||| simplestFloat ||| noBorders Full) $

      (avoidStruts $  tiled
                  ||| Mirror tiled
                  ||| Grid
                  ||| mySpiral
                  ||| simpleCross
                  ||| Circle
                  ||| centerMaster Grid
                  ||| tabbedLayout)
      ||| simplestFloat
      ||| noBorders Full

        where
          tiled        = Tall 1 delta ration
          ration       = 2/3 -- master proportion
          delta        = 3/100 -- percent of master resize
          tabbedLayout = tabbed shrinkText myTabTheme
          mySpiral     = spiral (6/7)

          lastWorkspacesLayouts = avoidStruts
                                $  simpleCross
                               ||| Circle
                               ||| centerMaster Grid
                               ||| tabbedLayout

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

myKeys customConfig =
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


  , ((myMetaKey, xK_p), spawn (cmd $ cfgLauncher      customConfig))
  , ((myMetaKey, xK_f), spawn (cmd $ cfgFileManager   customConfig))
  , ((myMetaKey, xK_d), spawn (cmd $ cfgTerminalDark  customConfig))
  , ((myMetaKey, xK_s), spawn (cmd $ cfgTerminalLight customConfig))

  , ((0, xF86XK_Calculator), spawn (cmd "gnome-calculator"))

  -- close focused window with optional shift modifier
  , ((myMetaKey, xK_slash), kill)

  , ((myMetaKey .|. shiftMask, xK_grave),   io exitSuccess)
  , ((myMetaKey, xK_grave), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  , ((myMetaKey .|. mod1Mask,  xK_space),   asks config >>= setLayout . layoutHook)
  , ((myMetaKey .|. shiftMask, xK_space),   asks config >>= setLayout . layoutHook)
  , ((myMetaKey,               xK_space),   sendMessage NextLayout)

  , ((myMetaKey .|. mod1Mask,  xK_j),       windows W.swapDown)
  , ((myMetaKey .|. shiftMask, xK_j),       windows W.swapDown)
  , ((myMetaKey .|. mod1Mask,  xK_k),       windows W.swapUp)
  , ((myMetaKey .|. shiftMask, xK_k),       windows W.swapUp)
  , ((myMetaKey .|. mod1Mask,  xK_Down),    windows W.swapDown)
  , ((myMetaKey .|. shiftMask, xK_Down),    windows W.swapDown)
  , ((myMetaKey .|. mod1Mask,  xK_Up),      windows W.swapUp)
  , ((myMetaKey .|. shiftMask, xK_Up),      windows W.swapUp)
  , ((myMetaKey, xK_Down),                  windows W.focusDown)
  , ((myMetaKey, xK_Up),                    windows W.focusUp)

  -- TODO only hidden workspace
  , ((myMetaKey, xK_Left),                  prevWS)
  , ((myMetaKey, xK_Right),                 nextWS)

  , ((myMetaKey .|. mod1Mask,  xK_Left),    prevWS)
  , ((myMetaKey .|. mod1Mask,  xK_Right),   nextWS)

  -- move windows
  , ((myMetaKey .|. shiftMask,   xK_Left),  shiftToPrev)
  , ((myMetaKey .|. shiftMask,   xK_Right), shiftToNext)

  , ((myMetaKey .|. controlMask, xK_Up),    sendMessage (IncMasterN 1))
  , ((myMetaKey .|. controlMask, xK_Down),  sendMessage (IncMasterN (-1)))
  , ((myMetaKey .|. controlMask, xK_Left),  sendMessage Shrink)
  , ((myMetaKey .|. controlMask, xK_Right), sendMessage Expand)
  ]

  ++

  -- move between displays by x,c,v keys
  let order = map screenNum $ cfgDisplaysOrder customConfig
      screenNum :: Int -> ScreenId
      screenNum x = [0..] !! (x-1)
  in
  [((m .|. myMetaKey, k), screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_x, xK_c, xK_v] order
        , (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  -- move between workspaces
  let keys1 = [ xK_u, xK_i, xK_o
              , xK_8, xK_9, xK_0
              , xK_minus, xK_equal
              ]
      -- support https://github.com/unclechu/X11-my-custom-layouts
      keys2 = [ xK_u, xK_i, xK_o
              , xK_asterisk, xK_parenleft, xK_parenright
              , xK_minus, xK_equal
              ]

      keys3 = [ xK_q, xK_w, xK_e
              , xK_1, xK_2, xK_3
              , xK_4, xK_5
              ]
      -- support https://github.com/unclechu/X11-my-custom-layouts
      keys4 = [ xK_q, xK_w, xK_e
              , xK_exclam, xK_at, xK_numbersign
              , xK_dollar, xK_percent
              ]

      keys5 = map numpadHackMap [ 1..8 ]
        where numpadHackMap x =
                case x of
                     0 -> xK_KP_Insert
                     1 -> xK_KP_End
                     2 -> xK_KP_Down
                     3 -> xK_KP_Next
                     4 -> xK_KP_Left
                     5 -> xK_KP_Begin
                     6 -> xK_KP_Right
                     7 -> xK_KP_Home
                     8 -> xK_KP_Up
                     9 -> xK_KP_Prior
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
        | k <- [ xK_6 .. xK_7 ]
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


-- TODO implement independent workspaces
data Config =
  Config { cfgIndependentWorkspaces :: Bool
         , cfgDisplaysOrder         :: [Int]
         , cfgMetaKey               :: KeyMask
         , cfgTerminal              :: String
         , cfgTerminalDark          :: String
         , cfgTerminalLight         :: String
         , cfgFileManager           :: String
         , cfgLauncher              :: String
         } deriving Show

defaultCustomConfig =
  Config { cfgIndependentWorkspaces = False
         , cfgDisplaysOrder         = [1,2,3]
         , cfgMetaKey               = mod4Mask
         , cfgTerminal              = "terminator"
         , cfgTerminalDark          = "terminator --profile dark"
         , cfgTerminalLight         = "terminator --profile light"
         , cfgFileManager           = "nautilus"
         , cfgLauncher              = "gmrun"
         }

-- example of config.txt (all keys are optional, see defaultCustomConfig):
-- independent-workspaces = yes
-- displays-order = 3,2,1
configFile :: IO String
configFile = fmap (++ "/.xmonad/config.txt") getHomeDirectory

parseCustomConfig config configFromFile =
  case configFromFile of
    Nothing -> config
    Just x  -> resolvePairs config
             . pairs
             . lines
             $ configFromFile
  where isSpaceSym :: Char -> Bool
        isSpaceSym x = x == ' ' || x == '\t'
        lines :: Maybe String -> [String]
        lines (Just x)
                    | x == ""   = []
                    | otherwise = foldr reducer [""] x
                    where reducer symbol (x:xs)
                            | symbol == '\n' = "":x:xs
                            | otherwise      = (symbol:x):xs
        pairs :: [String] -> [(String, String)]
        pairs = map checkForAvailableKey
              . foldr splitReducer []
              . filter (/= "")
              . map clearSidesSpaces
          where clearSidesSpaces = reverse
                                 . dropWhile isSpaceSym
                                 . reverse
                                 . dropWhile isSpaceSym
                splitReducer line acc = (k, v) : acc
                  where k = clearSidesSpaces $ takeWhile (/= '=') line
                        v = clearSidesSpaces $ tail $ dropWhile (/= '=') line
                checkForAvailableKey (k, v)
                  | k `elem` availableKeys = (k, v)
                  | otherwise = error "Unknown config key"
                  where availableKeys = [ "independent-workspaces"
                                        , "displays-order"
                                        , "terminal"
                                        , "terminal-dark"
                                        , "terminal-light"
                                        , "file-manager"
                                        , "launcher"
                                        ]
        resolvePairs :: Config -> [(String, String)] -> Config
        resolvePairs config [] = config
        resolvePairs config ((k, v):pairs) = resolvePairs newConfig pairs
          where newConfig = case k of
                  "independent-workspaces" ->
                    let nv = map toLower v
                    in case nv of
                         "yes" -> config { cfgIndependentWorkspaces = True  }
                         "no"  -> config { cfgIndependentWorkspaces = False }
                         _ -> error "Unexpected value of independent-workspaces in config"
                  "displays-order" ->
                    let cleanStr = filter (not . isSpaceSym) v
                        isValidSym x = x `elem` ',':['0'..'9']
                        validStr
                          | all isValidSym cleanStr = cleanStr
                          | otherwise = error "Unexpected value of displays-order in config"
                        listReducer :: Char -> [String] -> [String]
                        listReducer ',' acc = "":acc
                        listReducer c (x:xs) = (c:x):xs
                        order
                          | length result >= 2 = result
                          | otherwise = error "displays-order should have at least 2 items"
                          where result = map read
                                       $ foldr listReducer [""] validStr
                    in config { cfgDisplaysOrder = order }
                  "terminal"       -> config { cfgTerminal      = v }
                  "terminal-dark"  -> config { cfgTerminalDark  = v }
                  "terminal-light" -> config { cfgTerminalLight = v }
                  "file-manager"   -> config { cfgFileManager   = v }
                  "launcher"       -> config { cfgLauncher      = v }

getCustomConfig :: IO Config
getCustomConfig = parseCustomConfig defaultCustomConfig <$> readConfigFile
  where readConfigFile = do
          filePath <- configFile
          Exception.catch (Just <$> readFile filePath) handleExists
        handleExists e
          | Error.isDoesNotExistError e = return Nothing
          | otherwise = Exception.throwIO e

main :: IO ()
main = do

  customConfig <- getCustomConfig

  let conf = myConfig customConfig
      keys = myKeys   customConfig

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
      where
        showNamedWorkspaces wsId = wsId
        layoutNameHandler x = wrap $ xmobarEscape $ case x of
          "Tall"            -> "[>]"
          "Mirror Tall"     -> "[v]"
          "Grid"            -> "[+]"
          "Spiral"          -> "[0]"
          "Tabbed Simplest" -> "[t]"
          "Cross"           -> "[x]"
          "Circle"          -> "[o]"
          "SimplestFloat"   -> "[f]"
          "Full"            -> "[ ]"
          _                 ->   x
          where wrap t = "<action=xdotool key super+space>" ++ t ++ "</action>"
