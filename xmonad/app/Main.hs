-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import "xmonad" XMonad (xmonad, logHook)

import "xmonad-contrib" XMonad.Util.Run (spawnPipe)
import "xmonad-contrib" XMonad.Util.EZConfig (additionalKeys)

import qualified "xmonad-contrib" XMonad.Hooks.DynamicLog as DL
import "xmonad-contrib" XMonad.Hooks.FadeInactive
  ( fadeInactiveLogHook
  , fadeInactiveCurrentWSLogHook
  )

import "base" System.IO (hPutStrLn)

import "data-default" Data.Default (def)

import "unclechu-xmonadrc" Workspaces (myWorkspaces)
import "unclechu-xmonadrc" Config (myConfig)
import "unclechu-xmonadrc" Keys (myKeys)
import "unclechu-xmonadrc" Utils (xmobarEscape)
import "unclechu-xmonadrc" Utils.IPC (initIPC, deinitIPC)
import "unclechu-xmonadrc" Utils.CustomConfig
  ( getCustomConfig
  , Config ( cfgInactiveWindowOpacity
           , cfgInactiveWindowOpacityOnlyForCurrentWs
           )
  )


main :: IO ()
main = do

  customConfig <- getCustomConfig
  ipc          <- initIPC

  let conf = myConfig customConfig
      keys = myKeys ipc myWorkspaces customConfig

  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.generated.hs"

  xmonad $ conf
    { logHook = do

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

  deinitIPC ipc

  where layoutNameHandler :: String -> String
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
