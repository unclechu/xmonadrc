-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Utils.FocusHook
  ( focusHookConfig
  ) where

import XMonad ( XConfig
              , className, title
              , composeAll
              , (-->), (<+>), (=?), (<&&>), (<||>)
              )
import XMonad.Hooks.Focus ( FocusHook
                          , handleFocusQuery

                          , activated
                          , keepFocus
                          , switchFocus
                          , switchWorkspace
                          , new
                          , focused
                          )
import XMonad.Hooks.ManageHelpers (isDialog)

import Data.Default (def)
import Data.Maybe (Maybe(Just, Nothing))


-- https://github.com/sgf-dma/xmonad-contrib/blob/b79a12681f908b5a523d8d0e848d910b4bb8a9b5/XMonad/Hooks/Focus.hs
composeOne :: (Monoid a, Monad m) => [m (Maybe a)] -> m a
composeOne [] = return mempty
composeOne (mx : xs) = do
    x <- mx
    case x of
      Just y  -> return y
      Nothing -> composeOne xs

-- https://github.com/sgf-dma/xmonad-contrib/blob/b79a12681f908b5a523d8d0e848d910b4bb8a9b5/XMonad/Hooks/Focus.hs
infixr 0 -?>
(-?>) :: Monad m => m Bool -> m a -> m (Maybe a)
(-?>) mb mx     = do
    b <- mb
    if b
      then Just <$> mx
      else return Nothing


focusHookConfig :: XConfig a -> XConfig a
focusHookConfig = handleFocusQuery Nothing myManageHook
  where myManageHook = composeOne
          [ activated -?> activateFocusHook
          , Just <$> newFocusHook
          ]


newFocusHook :: FocusHook
newFocusHook = composeOne $

  raiseNewAndKeep [ className =? "Gmrun"
                  , title     =? "gpaste-zenity"
                  , className =? "Gnome-calculator"
                  ]
  ++
  withDialogs [ className =? "Firefox"
              , className =? "Tor Browser"

              -- Prevent lost focus for all messangers
              -- but allow dialog windows of these applications
              -- to grab focus.
              , className =? "Gajim"
              , className =? "Hexchat"
              , className =? "utox"
              , className =? "qTox"
              , className =? "Gnome-ring"
              , className =? "Riot"

              , className =? "Keepassx"
              ]
  ++
  -- Default behavior for new window, just usual switching focus.
  [ return True -?> switchFocus ]

  where withDialogs = foldr ((++) . f) []
          where f c = [ new (c <&&> isDialog) -?> switchFocus
                      , focused c             -?> keepFocus ]

        -- Always switch to new window even
        -- if focus is kept by another window
        -- and keep focus for this window.
        raiseNewAndKeep = foldr ((++) . f) []
          where f c = [ new     c -?> switchFocus
                      , focused c -?> keepFocus ]


activateFocusHook :: FocusHook
activateFocusHook = composeAll $
  keepFocusFor [ className =? "Gmrun"

               , className =? "Firefox"
               , className =? "Tor Browser"

               -- Prevent lost focus for all messangers
               , className =? "Gajim"
               , className =? "Hexchat"
               , className =? "utox"
               , className =? "qTox"
               , className =? "Gnome-ring"
               , className =? "Riot"

               , className =? "Keepassx"
               , title     =? "gpaste-zenity"
               ]
  ++
  [ return True --> switchWorkspace <+> switchFocus ]
  where keepFocusFor = foldr ((:) . f) []
        f cond = focused cond --> keepFocus
