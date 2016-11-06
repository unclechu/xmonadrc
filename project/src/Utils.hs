-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Utils
  ( doRepeat
  , xmobarEscape
  ) where

import Data.List as L


doRepeat :: (Monad a) => Int -> a () -> a ()
doRepeat c ff = repeat c ff
  where repeat c f | c == 1    = f
                   | otherwise = repeat (c - 1) $ f >> ff

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]
        concatMap :: (Char -> String) -> String -> String
        concatMap = L.concatMap
