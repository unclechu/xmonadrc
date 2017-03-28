-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Utils
  ( getTmpDBusSocketPath
  ) where

import "process" System.Process (readProcess)


getTmpDBusSocketPath :: IO String
getTmpDBusSocketPath = do

  (lines -> head -> tmpSocketPath) <-
    (\args -> readProcess "mktemp" args "")
      [ "--dry-run"
      , "--suffix=--unclechu-xmobar-indicators-cmd--testing-dbus-socket"
      ]

  return tmpSocketPath
