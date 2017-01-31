-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module FocusHook (focusManageHook) where

import Data.Default (def)

import XMonad ( XConfig
              , className, title
              , (-->), (<+>), (=?), (<&&>), (<||>)
              , ManageHook
              , composeAll
              )
import XMonad.Hooks.Focus ( FocusHook

                          , keepFocus
                          , switchFocus
                          , switchWorkspace
                          , new
                          , focused
                          , liftQuery
                          , manageFocus
                          )
import XMonad.Hooks.ManageHelpers (isDialog, composeOne, (-?>))
import XMonad.Hooks.EwmhDesktops (activated)


focusManageHook :: ManageHook
focusManageHook = manageFocus
                $ composeOne [ liftQuery activated -?> activateFocusHook
                             , Just <$> newFocusHook ]


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
              , className =? "Rambox"

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
               , className =? "Rambox"

               , className =? "Keepassx"
               , title     =? "gpaste-zenity"
               ]
  ++
  [ return True --> switchWorkspace <+> switchFocus ]

  where keepFocusFor = foldr ((:) . f) []
        f cond = focused cond --> keepFocus