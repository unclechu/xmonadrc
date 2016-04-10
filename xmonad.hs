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

import System.Directory (getHomeDirectory)
import qualified System.IO.Error as Error
import qualified Control.Exception as Exception
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Data.Char (toLower)

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspacesBareList :: [String]
myWorkspacesBareList  = [ "u","i","o", "8","9","0", "-",    "=" ]
myWorkspacesKeysList :: [String]
myWorkspacesKeysList  = [ "u","i","o", "8","9","0", "minus","equal" ]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ myWorkspacesBareList
  where
    clickable l = [ "<action=xdotool key super+" ++ k ++ ">" ++ ws ++ "</action>"
                  | (k, ws) <- zip myWorkspacesKeysList l ]

myManageHook :: ManageHook
myManageHook =  composeAll
  [ title    =? "gpaste-zenity"             --> doCenterFloat
  , title    =? "File Operation Progress"   --> doCenterFloat
  , title    =? "Copying files"             --> doCenterFloat
  , title    =? "Compress"                  --> doCenterFloat
  , role     =? "gimp-toolbox-color-dialog" --> doCenterFloat
  ]
    where role = stringProperty "WM_WINDOW_ROLE"

myConfig customConfig = defaultConfig
  { manageHook  = manageDocks <+> myManageHook
  , layoutHook  = myLayoutHook

  , borderWidth = 1

  , modMask     = cfgMetaKey customConfig
  , terminal    = cfgTerminal customConfig
  , workspaces  = myWorkspaces
  }
  where
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

  , ((myMetaKey .|. controlMask, xK_q), io exitSuccess)

  , ((myMetaKey .|. mod1Mask, xK_space), asks config >>= setLayout . layoutHook)
  , ((myMetaKey,              xK_space), sendMessage NextLayout)

  , ((myMetaKey .|. mod1Mask, xK_j),     windows W.swapDown)
  , ((myMetaKey .|. mod1Mask, xK_k),     windows W.swapUp)
  , ((myMetaKey .|. mod1Mask, xK_Right), windows W.swapDown)
  , ((myMetaKey .|. mod1Mask, xK_Down),  windows W.swapDown)
  , ((myMetaKey .|. mod1Mask, xK_Left),  windows W.swapUp)
  , ((myMetaKey .|. mod1Mask, xK_Up),    windows W.swapUp)
  , ((myMetaKey, xK_Right),              windows W.focusDown)
  , ((myMetaKey, xK_Down),               windows W.focusDown)
  , ((myMetaKey, xK_Left),               windows W.focusUp)
  , ((myMetaKey, xK_Up),                 windows W.focusUp)
  ]

  ++

  -- move between displays by x,c,v keys
  let order = map screenNum $ cfgDisplaysOrder customConfig
      screenNum :: Int -> ScreenId
      screenNum x = case x of
                         1 -> 0
                         2 -> 1
                         3 -> 2
  in [((m .|. myMetaKey, k), screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_x, xK_c, xK_v] $ order
        , (f, m)  <- [(W.view, 0), (W.shift, mod1Mask)]
        ]

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
configFile = getHomeDirectory >>= return . (++ "/.xmonad/config.txt")

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
          "SimplestFloat"   -> "[f]"
          "Full"            -> "[ ]"
          _                 ->   x
          where wrap t = "<action=xdotool key super+space>" ++ t ++ "</action>"
