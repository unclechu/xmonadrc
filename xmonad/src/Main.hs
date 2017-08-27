-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import "xmonad" XMonad (xmonad, logHook, (<+>))

import "xmonad-contrib" XMonad.Util.Run (spawnPipe)
import "xmonad-contrib" XMonad.Util.EZConfig (additionalKeys, additionalKeysP)

import "xmonad-contrib" XMonad.Hooks.EwmhDesktops (ewmh)
import qualified "xmonad-contrib" XMonad.Hooks.DynamicLog as DL

import "base" System.IO (hPutStrLn)

import "data-default" Data.Default (def)

import Workspaces (myWorkspaces)
import Config (myConfig)
import Keys (myKeys, myEZKeys)
import Utils (xmobarEscape)
import Utils.IPC (initIPC, deinitIPC)
import Utils.CustomConfig (getCustomConfig)


main :: IO ()
main = do

  customConfig <- getCustomConfig
  ipc          <- initIPC

  let conf   = myConfig customConfig
      keys   = myKeys ipc myWorkspaces customConfig
      ezKeys = myEZKeys ipc myWorkspaces customConfig

  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.generated.hs"

  xmonad $ ewmh $ conf { logHook = xmobarLogHook xmproc <+> logHook conf
                       } `additionalKeys` keys
                         `additionalKeysP` ezKeys

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

        xmobarLogHook xmproc =
          DL.dynamicLogWithPP $ def
            { DL.ppOutput  = hPutStrLn xmproc
            , DL.ppTitle   = DL.xmobarColor "gray" "#444" . DL.wrap " " " "
            , DL.ppCurrent = DL.xmobarColor "green" ""    . DL.wrap "[" "]"
            , DL.ppSep     = "  "
            , DL.ppWsSep   = " "
            , DL.ppLayout  = DL.xmobarColor "yellow" "" . layoutNameHandler
            , DL.ppHiddenNoWindows = id
            }
