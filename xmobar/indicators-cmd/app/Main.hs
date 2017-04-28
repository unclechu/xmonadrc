-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import "data-default" Data.Default (Default, def)
import "base" Data.List (intercalate)
import "base" Data.Bool (bool)

import "base" Control.Monad (when)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "base" System.IO (hPutStrLn, hFlush, stdout)
import "base" System.Exit (die, exitSuccess)

import "unix" System.Posix.Signals ( installHandler
                                   , Handler(Catch)
                                   , sigHUP
                                   , sigINT
                                   , sigTERM
                                   , sigPIPE
                                   )

import "dbus" DBus ( objectPath_
                   , busName_
                   , ObjectPath
                   , InterfaceName
                   , Signal(signalBody, signalSender, signalDestination)
                   , IsVariant(fromVariant)
                   , signal
                   )

import "dbus" DBus.Client ( connectSession
                          , disconnect
                          , requestName
                          , releaseName
                          , RequestNameReply(NamePrimaryOwner)
                          , addMatch
                          , removeMatch
                          , matchAny
                          , emit

                          , MatchRule ( matchPath
                                      , matchSender
                                      , matchDestination
                                      , matchInterface
                                      , matchMember
                                      )
                          )

import "X11" Graphics.X11.Xlib ( Display
                               , openDisplay
                               , closeDisplay
                               , displayString
                               )

-- local imports

import "unclechu-xmobar-indicators-cmd" ParentProc (dieWithParent)


data State = State { numLock     :: Bool
                   , capsLock    :: Bool
                   , alternative :: Bool
                   , focusLock   :: Bool
                   }
                     deriving (Show, Eq)

instance Default State where
  def = State { numLock     = False
              , capsLock    = False
              , alternative = False
              , focusLock   = False
              }

objPath :: ObjectPath
objPath = "/"

flushObjPathPfx :: String
flushObjPathPfx = "/com/github/unclechu/xmonadrc/"

-- To add current Display suffix
busNamePfx :: String
busNamePfx = "com.github.unclechu.xmonadrc."

interfaceName :: InterfaceName
interfaceName = "com.github.unclechu.xmonadrc"


view :: State -> String
view s = intercalate " " $
  map (\f -> f s) [numLockView, capsLockView, alternativeView, focusLockView]

  where numLockView (numLock -> isOn) =
          withAction "xdotool key Num_Lock" $
            colored (bool "#999" "#eee") (const "num") isOn

        capsLockView (capsLock -> isOn) =
          withAction "xdotool key Caps_Lock" $
            colored (bool "#999" "orange") (bool "caps" "CAPS") isOn

        -- TODO toggle it by mouse click (notify 'xlib-keys-hack')
        alternativeView (alternative -> isOn) =
          colored (bool "#999" "yellow") (bool "hax" "HAX") isOn

        focusLockView (focusLock -> isOn) =
          withAction "xdotool key super+y" $
            colored (bool "#999" "aqua") (bool "[ ]" "[x]") isOn

        withAction action x = "<action=" ++ action ++ ">" ++ x ++ "</action>"

        colored fColor fTitle isOn =
          "<fc=" ++ fColor isOn ++ ">" ++ fTitle isOn ++ "</fc>"


main :: IO ()
main = do
  -- Connecting to DBus
  client  <- connectSession

  -- Getting bus name for our service that depends on Display name
  dpyView <- do dpy <- openDisplay ""
                let x = getDisplayName dpy
                x <$ (x `seq` closeDisplay dpy)

  let busName = busName_ $ busNamePfx ++ dpyView
      flushObjPath = objectPath_ $ flushObjPathPfx ++ dpyView

  -- Grab the bus name for our service
  requestName client busName [] >>= \reply ->
    when (reply /= NamePrimaryOwner) $
      die $ "Requesting name '" ++ show busName ++ "' error: " ++ show reply

  mVar <- newEmptyMVar

  let put = putMVar mVar

      basicMatchRule = matchAny { matchPath        = Just objPath
                                , matchInterface   = Just interfaceName
                                , matchDestination = Just busName
                                , matchSender      = Nothing
                                }

  -- If `xlib-keys-hack` started before ask it to reflush indicators
  emit client (signal flushObjPath interfaceName "request_flush_all")
                { signalSender = Just busName
                , signalDestination = Nothing
                , signalBody = []
                }

  sigHandlers <-
    let listen (member, lens) = addMatch client (matchRule member) $ handle lens
        matchRule member = basicMatchRule { matchMember = Just member }

        handle lens (signalBody -> map fromVariant -> [Just (v :: Bool)]) =
          put $ Just (lens, v)

        handle _ _ = return () -- Incorrect arguments, just ignoring it

     in mapM listen [ ("numlock",     \s v -> s { numLock     = v })
                    , ("capslock",    \s v -> s { capsLock    = v })
                    , ("alternative", \s v -> s { alternative = v })
                    , ("focuslock",   \s v -> s { focusLock   = v })
                    ]

  let terminate = do mapM_ (removeMatch client) sigHandlers
                     _ <- releaseName client busName
                     disconnect client
                     put Nothing

      catch sig = installHandler sig (Catch terminate) Nothing

   in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent

  let handle :: State -> Maybe ((State -> Bool -> State), Bool) -> IO State
      handle prevState Nothing = prevState <$ exitSuccess

      handle prevState (Just (lens, v)) =
        let newState = lens prevState v
         in if newState == prevState
               then return prevState
               else newState <$ echo (view newState)

      next s = takeMVar mVar >>= handle s >>= next

   in () <$ echo (view def) >> next def


echo :: String -> IO ()
echo s = hPutStrLn stdout s >> hFlush stdout

getDisplayName :: Display -> String
getDisplayName dpy = map f $ displayString dpy
  where f ':' = '_'
        f '.' = '_'
        f  x  =  x
