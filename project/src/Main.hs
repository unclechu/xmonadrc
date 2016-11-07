-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Main (main) where

import qualified XMonad as XM
import XMonad ( (=?), (-->), (<&&>), (<+>), (|||)
              , Mirror(Mirror)
              , Full(Full)
              , Tall(Tall)
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

import System.IO (hPutStrLn)

import Utils (xmobarEscape)
import Utils.CustomConfig (getCustomConfig, Config(..))
import Keys (myKeys)


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
  , wmRole       =? "gimp-toolbox-color-dialog" --> doCenterFloat
  , wmRole       =? "gimp-message-dialog"       --> doCenterFloat
  , wmRole       =? "gimp-layer-new"            --> doCenterFloat
  , wmRole       =? "gimp-image-new"            --> doCenterFloat

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

          lastWorkspacesLayouts =  avoidStruts
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


main :: IO ()
main = do

  customConfig <- getCustomConfig

  let conf = myConfig customConfig
      keys = myKeys myWorkspaces
                    customConfig

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
