-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Utils
  ( getTmpDBusSocketPath
  ) where

import "process"   System.Process   (readProcess)
import "directory" System.Directory (removeFile)


getTmpDBusSocketPath :: IO String
getTmpDBusSocketPath = do

  (lines -> head -> tmpFilePath) <-
    (\args -> readProcess "mktemp" args "")
      ["-t", "unclechu-xmobar-indicators-cmd--testing-dbus-socket"]

  tmpFilePath <$ removeFile tmpFilePath
