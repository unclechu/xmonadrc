-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils
  ( doRepeat
  , xmobarEscape
  ) where

import "base" Data.List (concatMap)


doRepeat :: (Monad a) => Int -> a () -> a ()
doRepeat c ff = _repeat c ff
  where _repeat _c f | _c == 1   = f
                     | otherwise = _repeat (_c - 1) $ f >> ff


xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts  x  = [x]
