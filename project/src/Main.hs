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
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))

import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook, fadeInactiveCurrentWSLogHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import System.IO (hPutStrLn)
import qualified Data.Default
import qualified Data.Maybe as Maybe

import Utils (xmobarEscape)
import Utils.CustomConfig (getCustomConfig, Config(..))
import Keys (myKeys)
import Workspaces (myWorkspacesBareList, myWorkspaces)


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

  , XM.className =? "Firefox" <&&> nameStartsWith "Riot"
      --> moveTo (last myWorkspaces)

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
          wmName = XM.stringProperty "WM_NAME"
          moveTo = XM.doF . W.shift

          nameStartsWith :: String -> XM.Query Bool
          nameStartsWith startPart =
            fmap (take $ length startPart) wmName =? startPart


myConfig customConfig = Data.Default.def
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
      onWorkspace (myWorkspaces !! 2)        startWithCrossLayouts $
      usualLayouts
        where
          tiled        = Tall 1 delta ration
          ration       = 2/3 -- master proportion
          delta        = 1/100 -- percent of master resize
          tabbedLayout = Tabbed.tabbed Tabbed.shrinkText myTabTheme
          mySpiral     = spiral (6/7)
          rTiled       = ResizableTall 1 delta ration []

          usualLayouts =
            ( avoidStruts  $  rTiled
                          ||| Mirror rTiled
                          ||| Grid
                          ||| mySpiral
                          ||| simpleCross
                          ||| Circle
                          ||| centerMaster Grid
                          ||| tabbedLayout
                          ||| ThreeColMid 1 delta (1/2)
            ) ||| simplestFloat
              ||| noBorders Full

          startWithCrossLayouts =
            ( avoidStruts  $  simpleCross
                          ||| Circle
                          ||| centerMaster Grid
                          ||| tabbedLayout
                          ||| rTiled
                          ||| Mirror rTiled
                          ||| Grid
                          ||| mySpiral
            ) ||| simplestFloat
              ||| noBorders Full

          lastWorkspacesLayouts =
            avoidStruts  $  simpleCross
                        ||| Circle
                        ||| centerMaster Grid
                        ||| tabbedLayout


myTabTheme = Data.Default.def
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
      keys = myKeys myWorkspaces customConfig

  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"

  XM.xmonad $ conf
    { XM.logHook = do

        DL.dynamicLogWithPP $ Data.Default.def
          { DL.ppOutput  = hPutStrLn xmproc
          , DL.ppTitle   = DL.xmobarColor "gray" "#444" . DL.wrap " " " "
          , DL.ppCurrent = DL.xmobarColor "green" ""    . DL.wrap "[" "]"
          , DL.ppSep     = "  "
          , DL.ppWsSep   = " "
          , DL.ppLayout  = DL.xmobarColor "yellow" "" . layoutNameHandler
          , DL.ppHiddenNoWindows = showNamedWorkspaces
          }

        let inactiveOpacity = cfgInactiveWindowOpacity customConfig
        if cfgInactiveWindowOpacityOnlyForCurrentWs customConfig
           then fadeInactiveCurrentWSLogHook inactiveOpacity
           else fadeInactiveLogHook inactiveOpacity

    } `additionalKeys` keys
      where
        showNamedWorkspaces wsId = wsId
        layoutNameHandler :: String -> String
        layoutNameHandler x = wrap $ xmobarEscape $
          case x of
               "ResizableTall"        -> "[>]"
               "Mirror ResizableTall" -> "[v]"
               "Grid"                 -> "[+]"
               "Spiral"               -> "[0]"
               "Tabbed Simplest"      -> "[t]"
               "Cross"                -> "[x]"
               "Circle"               -> "[o]"
               "ThreeCol"             -> "[3]"
               "SimplestFloat"        -> "[f]"
               "Full"                 -> "[ ]"
               _                      ->   x
          where wrap t = "<action=xdotool key super+space>" ++ t ++ "</action>"
