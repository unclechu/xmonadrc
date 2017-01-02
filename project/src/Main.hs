-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Main (main) where

import qualified XMonad as XM
import XMonad ( (=?), (-->), (<&&>), (<||>), (<+>), (|||)

              , Mirror(Mirror)
              , Full(Full)
              , Tall(Tall)

              , ManageHook

              , composeAll
              , className, title, stringProperty
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
import XMonad.Hooks.FadeInactive ( fadeInactiveLogHook
                                 , fadeInactiveCurrentWSLogHook
                                 )
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import System.IO (hPutStrLn)
import System.Directory (getHomeDirectory)

import Data.Default (def)

import Utils (xmobarEscape)
import Utils.CustomConfig (getCustomConfig, Config(..))
import Utils.FocusHook (focusManageHook)
import Keys (myKeys)
import Workspaces (myWorkspacesBareList, myWorkspaces)


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


myConfig customConfig = def
  { XM.manageHook        = manageDocks <+> myManageHook <+> focusManageHook
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


main :: IO ()
main = do

  customConfig <- getCustomConfig
  homeDir <- getHomeDirectory

  let conf = myConfig customConfig
      keys = myKeys myWorkspaces customConfig homeDir

  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"

  XM.xmonad $ conf
    { XM.logHook = do

        DL.dynamicLogWithPP $ def
          { DL.ppOutput  = hPutStrLn xmproc
          , DL.ppTitle   = DL.xmobarColor "gray" "#444" . DL.wrap " " " "
          , DL.ppCurrent = DL.xmobarColor "green" ""    . DL.wrap "[" "]"
          , DL.ppSep     = "  "
          , DL.ppWsSep   = " "
          , DL.ppLayout  = DL.xmobarColor "yellow" "" . layoutNameHandler
          , DL.ppHiddenNoWindows = id
          }

        let inactiveOpacity = cfgInactiveWindowOpacity customConfig
         in if cfgInactiveWindowOpacityOnlyForCurrentWs customConfig
               then fadeInactiveCurrentWSLogHook inactiveOpacity
               else fadeInactiveLogHook inactiveOpacity

    } `additionalKeys` keys
      where
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
