-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE PackageImports #-}

module Config
  ( myConfig
  ) where

import "xmonad" XMonad ( (=?), (-->), (<&&>), (<+>), (|||)

                       , Mirror(Mirror)
                       , Full(Full)

                       , ManageHook

                       , composeAll
                       , className, title, stringProperty
                       )
import qualified "xmonad" XMonad as XM
import qualified "xmonad" XMonad.StackSet as W

import "xmonad-contrib" XMonad.Layout.Grid (Grid(Grid))
import "xmonad-contrib" XMonad.Layout.Spiral (spiral)
import "xmonad-contrib" XMonad.Layout.NoBorders (noBorders)
import "xmonad-contrib" XMonad.Layout.SimplestFloat (simplestFloat)
import "xmonad-contrib" XMonad.Layout.PerWorkspace (onWorkspace)
import "xmonad-contrib" XMonad.Layout.Cross (simpleCross)
import "xmonad-contrib" XMonad.Layout.Circle (Circle(Circle))
import "xmonad-contrib" XMonad.Layout.CenteredMaster (centerMaster)
import "xmonad-contrib" XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import "xmonad-contrib" XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import qualified "xmonad-contrib" XMonad.Layout.Tabbed as Tabbed

import "xmonad-contrib" XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import "xmonad-contrib" XMonad.Hooks.ManageHelpers (doCenterFloat)

import "data-default" Data.Default (def)

-- local imports

import Workspaces (myWorkspaces)
import FocusHook (focusManageHook)
import Utils.CustomConfig (Config(cfgMetaKey, cfgTerminal, cfgBorderWidth))


myConfig customConfig = def
  { XM.manageHook        = manageDocks <+> focusManageHook <+> myManageHook
  , XM.layoutHook        = myLayoutHook

  , XM.borderWidth       = read $ show $ cfgBorderWidth customConfig

  , XM.modMask           = cfgMetaKey customConfig
  , XM.terminal          = cfgTerminal customConfig
  , XM.workspaces        = myWorkspaces

  , XM.focusFollowsMouse = False
  , XM.clickJustFocuses  = True
  }
  where
    myLayoutHook =
      onWorkspace (last myWorkspaces)        secondaryLayouts $
      onWorkspace (last $ init myWorkspaces) secondaryLayouts $
      onWorkspace (myWorkspaces !! 2)        secondaryLayouts $
      usualLayouts
        where
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

          secondaryLayouts =
            ( avoidStruts  $  centerMaster Grid
                          ||| Grid
                          ||| rTiled
                          ||| Mirror rTiled
                          ||| mySpiral
                          ||| simpleCross
                          ||| Circle
                          ||| tabbedLayout
            ) ||| simplestFloat
              ||| noBorders Full


myManageHook :: ManageHook
myManageHook = composeAll $

  [ className =? "Gmrun"                     --> doCenterFloat

  , title     =? "gpaste-zenity"             --> doCenterFloat

  , className =? "Gnome-calculator"          --> doCenterFloat

  -- GIMP
  , wmRole    =? "gimp-toolbox-color-dialog" --> doCenterFloat
  , wmRole    =? "gimp-message-dialog"       --> doCenterFloat
  , wmRole    =? "gimp-layer-new"            --> doCenterFloat
  , wmRole    =? "gimp-image-new"            --> doCenterFloat

  , className =? "qjackctl"                  --> doCenterFloat
  , className =? "Audacious"                 --> moveTo (last $
                                                         init myWorkspaces)

  , className =? "Doublecmd"
      <&&> fmap not (nameContains "Double Commander ")
                                             --> doCenterFloat

  -- Move messangers to last workspace
  , className =? "Gajim"                     --> moveTo lastWs
  , className =? "Hexchat"                   --> moveTo lastWs
  , className =? "utox"                      --> moveTo lastWs
  , className =? "qTox"                      --> moveTo lastWs
  , className =? "Gnome-ring"                --> moveTo lastWs
  , className =? "Riot"                      --> moveTo lastWs
  , className =? "Rambox"                    --> moveTo lastWs
  ]

  ++

  -- Audacious
  [ className =? "Audacious" <&&> title =? x --> doCenterFloat
  | x <- [ "Song Info"
         , "Audacious Settings"
         , "JACK Output Settings"
         , "Add Files"
         , "Open Files"
         ]
  ]

    where wmRole = stringProperty "WM_WINDOW_ROLE"
          wmName = stringProperty "WM_NAME"
          moveTo = XM.doF . W.shift
          lastWs = last myWorkspaces

          nameContains :: String -> XM.Query Bool
          nameContains namePart = fmap f wmName
            where f :: String -> Bool
                  f x | x == ""            = False
                      | part x == namePart = True
                      | otherwise          = f $ tail x
                  len  = length namePart :: Int
                  part = take len        :: String -> String


myTabTheme :: Tabbed.Theme
myTabTheme = def
  { Tabbed.activeColor         = "#3c5863"
  , Tabbed.activeBorderColor   = "#000000"
  , Tabbed.inactiveColor       = "#666666"
  , Tabbed.inactiveBorderColor = "#000000"
  , Tabbed.activeTextColor     = "lightgray"
  , Tabbed.inactiveTextColor   = "#aaa"
  , Tabbed.decoHeight          = 12
  , Tabbed.fontName            = "terminus"
  }
