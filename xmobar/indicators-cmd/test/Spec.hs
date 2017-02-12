-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import "base" Control.Concurrent (threadDelay)
import "base" Control.Exception (finally)

import "base" System.Environment (getEnvironment)
import "base" System.Exit (ExitCode(ExitSuccess, ExitFailure))
import "base" System.IO (Handle, hGetLine)
import "unix" System.Posix.Signals ( Signal
                                   , signalProcess
                                   , sigHUP
                                   , sigINT
                                   , sigTERM
                                   , sigPIPE
                                   , sigKILL
                                   )
import "process" System.Process.Internals ( ProcessHandle__(OpenHandle)
                                          , withProcessHandle
                                          )
import "process" System.Process ( StdStream(NoStream, Inherit, CreatePipe)
                                , CreateProcess(std_in, std_out, std_err, env)
                                , ProcessHandle
                                , createProcess
                                , readProcess
                                , proc
                                , terminateProcess
                                , waitForProcess
                                )

import "HUnit" Test.HUnit ( Test(TestList, TestCase)
                          , runTestTT
                          , assertEqual
                          )
import "dbus" DBus (parseAddress, formatAddress, Address)
import "dbus" DBus.Client (Client, connect, disconnect)


tests :: Test
tests = TestList [ testTerminating sigHUP
                 , testTerminating sigINT
                 , testTerminating sigTERM
                 , testTerminating sigPIPE
                 , testKilling
                 ]


testTerminating :: Signal -> Test
testTerminating sig = TestCase $ withApp $ \_ pHandle pOut -> do

  withProcessHandle pHandle $ \(OpenHandle cpid) -> do
    _ <- hGetLine pOut
    signalProcess sig cpid

  waitForProcess pHandle
    >>= assertEqual
          ("Application terminates by " ++ show sig ++ " successfully")
          ExitSuccess

  return ()


testKilling :: Test
testKilling = TestCase $ withApp $ \_ pHandle pOut -> do

  withProcessHandle pHandle $ \(OpenHandle cpid) -> do
    _ <- hGetLine pOut
    signalProcess sigKILL cpid

  waitForProcess pHandle
    >>= assertEqual
          "Application died by killing as expected"
          (ExitFailure $ negate $ fromIntegral sigKILL)

  return ()


withApp :: (Client -> ProcessHandle -> Handle -> IO ()) -> IO ()
withApp m = withTmpDBus $ \client addr -> do

  envList <- filter ((/= "DBUS_SESSION_BUS_ADDRESS") . fst) <$> getEnvironment

  (lines -> head -> (++ "/bin") -> binDir) <-
    readProcess "stack" ["path", "--local-install-root"] ""

  (Nothing, Just pOut, _, pHandle) <-
    createProcess
      (proc (binDir ++ "/unclechu-xmobar-indicators-cmd") [])
        { std_in  = NoStream
        , std_out = CreatePipe
        , std_err = Inherit
        , env = Just $
            ("DBUS_SESSION_BUS_ADDRESS", formatAddress addr) : envList
        }

  m client pHandle pOut
    `finally` (terminateProcess pHandle >> waitForProcess pHandle)


withTmpDBus :: (Client -> Address -> IO ()) -> IO ()
withTmpDBus m = do

  (lines -> head -> tmpSocketPath) <-
    (\args -> readProcess "mktemp" args "")
      [ "--dry-run"
      , "--suffix=--unclechu-xmobar-indicators-cmd--testing-dbus-socket"
      ]

  let addrPath = "unix:path=" ++ tmpSocketPath

  (Nothing, _, _, pHandle) <-
    createProcess
      (proc "dbus-daemon" ["--address=" ++ addrPath, "--session", "--nofork"])
        { std_in  = NoStream
        , std_out = Inherit
        , std_err = Inherit
        }

  -- Wait 1 sec for starting dbus-daemon
  threadDelay $ 1000 * 1000

  let Just addr = parseAddress addrPath
  client <- connect addr

  m client addr
    `finally` (disconnect client >> terminateProcess pHandle)


main :: IO ()
main = do

  (Nothing, _, _, pHandle) <-
    createProcess
      (proc "stack" ["build", "unclechu-xmobar-indicators-cmd"])
        { std_in  = NoStream
        , std_out = Inherit
        , std_err = Inherit
        }

  ExitSuccess <- waitForProcess pHandle

  () <$ runTestTT tests
