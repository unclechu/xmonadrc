-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE
--
-- Special thanks to @kurnevsky for his config that helped a lot:
-- https://github.com/kurnevsky/dotfiles/blob/36b4cc1209f69441bcddaf87b9986e855528edc7/.xmonad/lib/XMonad/Util/Compton.hs
--

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.IPC
  ( IPCHandler
  , initIPC
  , deinitIPC

  , focusLockState
  , invertWindowColors
  ) where

import "base" Data.Word (Word16, Word32)
import "base" Data.Bool (bool)

import "base" Control.Monad ((>=>))

import "dbus" DBus ( BusName
                   , busName_
                   , InterfaceName
                   , Variant
                   , fromVariant
                   , toVariant
                   , signal
                   , signalBody
                   , signalDestination
                   , methodCall
                   , methodCallBody
                   , methodCallDestination
                   , methodReturnBody
                   )
import "dbus" DBus.Client ( Client
                          , connectSession
                          , disconnect
                          , emit
                          , call_
                          , callNoReply
                          )
import "X11" Graphics.X11.Xlib ( Display
                               , Window
                               , openDisplay
                               , closeDisplay
                               , displayString
                               )


data IPCHandler = IPCHandler { client     :: Client
                             , xmobarBus  :: BusName
                             , comptonBus :: BusName
                             }


initIPC :: IO IPCHandler
initIPC = do

  xDpyName <- do dpy <- openDisplay ""
                 let name = getXDpyName dpy
                 name `seq` name <$ closeDisplay dpy

  connectSession <&> \x ->

    IPCHandler { client     = x
               , xmobarBus  = busName_ $
                   "com.github.unclechu.xmonadrc." ++ xDpyName
               , comptonBus = busName_ $
                   "com.github.chjj.compton." ++ xDpyName
               }


deinitIPC :: IPCHandler -> IO ()
deinitIPC = disconnect . client


-- Updates focus lock indicator state
focusLockState :: IPCHandler -> Bool -> IO ()
focusLockState IPCHandler { client = c, xmobarBus = bus } state =

  emit c (signal "/" xmobarInterface "focuslock")
           { signalDestination = Just bus
           , signalBody        = [toVariant state]
           }


invertWindowColors :: IPCHandler -> Window -> IO ()
invertWindowColors ipc@IPCHandler { client = c, comptonBus = bus } wnd =

  getWindowColorsInversionStatus ipc wnd
    >>= maybe (return ()) (callNoReply c . mc)

  where mc (not -> bool 0 1 -> toStatus) =
           (methodCall "/" comptonInterface "win_set")
             { methodCallDestination = Just bus
             , methodCallBody =
                 [ toVariant (fromIntegral wnd     :: Word32)
                 , toVariant ("invert_color_force" :: String)
                 , toVariant (toStatus             :: Word16)
                 ]
             }

getWindowColorsInversionStatus :: IPCHandler -> Window -> IO (Maybe Bool)
getWindowColorsInversionStatus IPCHandler { client = c, comptonBus = bus } wnd =

  call_ c mc <&> methodReturnBody <&> extractStatus

  where mc = (methodCall "/" comptonInterface "win_get")
               { methodCallDestination = Just bus
               , methodCallBody =
                   [ toVariant (fromIntegral wnd     :: Word32)
                   , toVariant ("invert_color_force" :: String)
                   ]
               }

        extractStatus :: [Variant] -> Maybe Bool
        extractStatus = ifMaybe ((== 1) . length)
                          >=> fromVariant . head
                          >=> return . \case (1 :: Word16) -> True
                                             (_ :: Word16) -> False


comptonInterface :: InterfaceName
comptonInterface = "com.github.chjj.compton"

xmobarInterface :: InterfaceName
xmobarInterface = "com.github.unclechu.xmonadrc"

getXDpyName :: Display -> String
getXDpyName dpy = map f $ displayString dpy

  where f ':' = '_'
        f '.' = '_'
        f  x  =  x

-- Pipe version of `fmap` operator.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixr 4 <&>

ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x = if f x then Just x else Nothing
