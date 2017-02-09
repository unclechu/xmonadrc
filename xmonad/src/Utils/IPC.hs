-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.IPC
  ( IPCHandler
  , initIPC
  , deinitIPC
  , focusLockState
  ) where

import "dbus" DBus ( BusName
                   , busName_
                   , InterfaceName
                   , toVariant
                   , signal
                   , signalBody
                   , signalDestination
                   )
import "dbus" DBus.Client ( Client
                          , connectSession
                          , disconnect
                          , emit
                          )
import "X11" Graphics.X11.Xlib ( Display
                               , openDisplay
                               , closeDisplay
                               , displayString
                               )


data IPCHandler = IPCHandler { client        :: Client
                             , xmobarBusName :: BusName
                             }


initIPC :: IO IPCHandler
initIPC = do

  -- Getting bus name for our service that depends on Display name
  busName <- do dpy <- openDisplay ""
                let name = "com.github.unclechu.xmonadrc." ++ getXDpyName dpy
                    busName = busName_ name
                busName `seq` closeDisplay dpy
                return busName

  connectSession <&> \x -> IPCHandler { client        = x
                                      , xmobarBusName = busName
                                      }


deinitIPC :: IPCHandler -> IO ()
deinitIPC = disconnect . client


focusLockState :: IPCHandler -> Bool -> IO ()
focusLockState ipc state = emit (client ipc)
  (signal "/" xmobarInterfaceName "focuslock")
    { signalDestination = Just $ xmobarBusName ipc
    , signalBody        = [toVariant state]
    }


xmobarInterfaceName :: InterfaceName
xmobarInterfaceName = "com.github.unclechu.xmonadrc"

getXDpyName :: Display -> String
getXDpyName dpy = map f $ displayString dpy
  where f ':' = '_'
        f '.' = '_'
        f  x  =  x

-- Pipe version of `fmap` operator.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixr 4 <&>
