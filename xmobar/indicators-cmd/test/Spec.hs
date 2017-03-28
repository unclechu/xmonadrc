-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import "base" Data.Function ((&), fix)
import "base" Data.List (stripPrefix, find)
import "base" Data.Bool (bool)
import "base" Data.Maybe (isJust)

import "base" Control.Concurrent (threadDelay)
import "base" Control.Exception (finally)
import "base" Control.Monad (unless, forM_)

import "base" System.Environment (getEnvironment)
import "base" System.Exit (ExitCode(ExitSuccess, ExitFailure))
import "base" System.IO (Handle, hGetLine)
import "directory" System.Directory (doesFileExist)
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
                          , assertBool
                          )
import "dbus" DBus ( Address
                   , BusName
                   , busName_
                   , MemberName
                   , parseAddress
                   , formatAddress
                   , signal
                   , signalBody
                   , signalDestination
                   , toVariant
                   )
import "dbus" DBus.Client ( Client
                          , connect
                          , disconnect
                          , emit
                          )
import "X11" Graphics.X11.Xlib ( Display
                               , openDisplay
                               , closeDisplay
                               , displayString
                               )

-- local imports

import "unclechu-xmobar-indicators-cmd" Test.Utils (getTmpDBusSocketPath)


main :: IO ()
main = do
  successfullyExec "stack" ["clean", "unclechu-xmobar-indicators-cmd"]
  successfullyExec "stack" ["build", "unclechu-xmobar-indicators-cmd"]
  () <$ runTestTT tests

  where successfullyExec :: String -> [String] -> IO ()
        successfullyExec app args = do

          (Nothing, _, _, pHandle) <-
            createProcess (proc app args) { std_in  = NoStream
                                          , std_out = Inherit
                                          , std_err = Inherit
                                          }

          ExitSuccess <- waitForProcess pHandle
          return ()


tests :: Test
tests = TestList [ testTerminating sigHUP
                 , testTerminating sigINT
                 , testTerminating sigTERM
                 , testTerminating sigPIPE
                 , testKilling
                 , initialIndicators
                 , testCombinations
                 ]


initialIndicators :: Test
initialIndicators = TestCase $ withAppAndTerminate $ \_ _ pOut ->

  lines <$> hGetLine pOut
    >>= (\x -> head x <$ assertEqual "Only single line" 1 (length x))
    >>= return . getRidOfActions
    >>= assertEqual "Initial indicators is correct" shouldBe

  where shouldBe = renderIndicators [ ("#999", "num")
                                    , ("#999", "caps")
                                    , ("#999", "hax")
                                    , ("#999", "[ ]")
                                    ]


type CombinationRecord = (MemberName, Bool, (String -> Bool), String)
type Combination = [CombinationRecord]
type IndicatorsState = [(MemberName, Bool)]
testCombinations :: Test
testCombinations = TestCase $ withAppAndTerminate $ \client busName pOut -> do

  (getRidOfActions -> parseIndicators -> indicatorsAtStart) <- hGetLine pOut

  let changeState :: MemberName -> Bool -> IO ()
      changeState member state =
        emit client (signal "/" "com.github.unclechu.xmonadrc" member)
                      { signalDestination = Just busName
                      , signalBody        = [toVariant state]
                      }

      handle :: [Combination] -> IndicatorsState -> [Indicator] -> IO ()
      handle [] _ _ = return ()

      handle (x:xs) state indicators = do

        (newState, newIndicators) <-
          handleRecords (getDiffs state x) state indicators

        assertEqual "Combinations count is correct" (length state) (length x)
        assertEqual "Indicators count is correct" (length indicators) (length x)

        forM_ (zip3 x newState newIndicators) $
          \((cMember, cIsOn, cf, cTitle), (sMember, sIsOn), (iColor, iTitle)) -> do
            assertEqual "Member matches internal state" cMember sMember
            assertEqual "State of indicator matches internal state" cIsOn sIsOn
            assertEqual "Title of indicator is correct" cTitle iTitle
            assertBool "Color of indicator is correct" $ cf iColor

        handle xs newState newIndicators

      handleRecords :: [CombinationRecord]
                    -> IndicatorsState
                    -> [Indicator]
                    -> IO (IndicatorsState, [Indicator])

      handleRecords [] state indicators = return (state, indicators)
      handleRecords ((member, isOn, _, _) : xs) state _ = do

        let newState = map f state where f (a, b) | a == member = (a, isOn)
                                                  | otherwise   = (a, b)

        changeState member isOn

        lines <$> hGetLine pOut
          >>= (\x -> head x <$ assertEqual "Only single line" 1 (length x))
          >>= return . parseIndicators . getRidOfActions
          >>= (\x -> x <$ assertEqual "Indicators count is correct"
                                      (length newState)
                                      (length x)
              )
          >>= handleRecords xs newState

   in flip (handle combinations) indicatorsAtStart [ ("numlock",     False)
                                                   , ("capslock",    False)
                                                   , ("alternative", False)
                                                   , ("focuslock",   False)
                                                   ]

  where -- All combinations of all possible states
        -- to check it one by one.
        combinations :: [Combination]
        combinations = let cf = bool (== "#999") (/= "#999") in
          [
            [ ("numlock",     num,   cf num,   "num")
            , ("capslock",    caps,  cf caps,  caps  ? "CAPS" $ "caps")
            , ("alternative", alt,   cf alt,   alt   ? "HAX"  $ "hax")
            , ("focuslock",   focus, cf focus, focus ? "[x]"  $ "[ ]")
            ]

            | num   <- [True, False]
            , caps  <- [True, False]
            , alt   <- [True, False]
            , focus <- [True, False]
          ]

        getDiffs :: IndicatorsState -> Combination -> [CombinationRecord]
        getDiffs state = filter $ \(ca, cb, _, _) ->
          isJust $ find (\(sa, sb) -> ca == sa && cb /= sb) state


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



withAppAndTerminate :: (Client -> BusName -> Handle -> IO ()) -> IO ()
withAppAndTerminate m = withApp $ \client pHandle pOut -> do

  xDpyName <- do dpy <- openDisplay ""
                 let name = getXDpyName dpy
                 name `seq` name <$ closeDisplay dpy

  let busName = busName_ $ "com.github.unclechu.xmonadrc." ++ xDpyName

  m client busName pOut `finally` do
    terminateProcess pHandle
    waitForProcess pHandle
      >>= assertEqual "Application terminated successfully" ExitSuccess


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

  tmpSocketPath <- getTmpDBusSocketPath
  let addrPath = "unix:path=" ++ tmpSocketPath

  (Nothing, _, _, pHandle) <-
    createProcess
      (proc "dbus-daemon" ["--address=" ++ addrPath, "--session", "--nofork"])
        { std_in  = NoStream
        , std_out = Inherit
        , std_err = Inherit
        }

  -- Wait for DBus socket readiness
  fix $ \wait ->
    doesFileExist tmpSocketPath
      >>= flip unless (threadDelay 100000 >> wait)

  let Just addr = parseAddress addrPath
  client <- connect addr

  m client addr
    `finally` (disconnect client >> terminateProcess pHandle)


-- Removes <action> wrappers
getRidOfActions :: String -> String
getRidOfActions s = parse (s, "")

  where parse :: (String, String) -> String
        parse ("", to) = reverse to

        parse ((stripPrefix "<action=" -> Just rest), to) =
          let (afterAction, insideAction) = actionOpen rest
           in parse (afterAction, (insideAction ++ to))

        parse (x:xs, to) = parse (xs, x:to)

        actionOpen :: String -> (String, String)
        actionOpen ""       = actionNotClosedErr
        actionOpen ('>':xs) = actionClose (xs, "")
        actionOpen (_:xs)   = actionOpen xs

        actionClose :: (String, String) -> (String, String)
        actionClose ("", _) = actionNotClosedErr
        actionClose ((stripPrefix "</action>" -> Just rest), to) = (rest, to)
        actionClose (x:xs, to) = actionClose (xs, x:to)

        actionNotClosedErr = error $
          "Parse error (<action> opened but not closed) in: '" ++ s ++ "'"


type Color     = String
type Title     = String
type Indicator = (Color, Title)

parseIndicators :: String -> [Indicator]
parseIndicators s = parse (' ':s, []) -- Add space before for generic handle

  where parse :: (String, [Indicator]) -> [Indicator]
        parse ("", to) = reverse $ map (\(a, b) -> (reverse a, reverse b)) to

        parse ((stripPrefix " <fc=" -> Just rest), to) =
          let (afterFc, indicator) = fcOpen (rest, "")
           in parse (afterFc, indicator : to)

        parse _ = incorrectIndicatorsErr

        fcOpen :: (String, Color) -> (String, Indicator)
        fcOpen ("", _) = fcNotClosedErr
        fcOpen ('>':xs, color) = fcClose (xs, "") & \(a, to) -> (a, (color, to))
        fcOpen (x:xs, color) = fcOpen (xs, x : color)

        fcClose :: (String, String) -> (String, String)
        fcClose ("", _) = fcNotClosedErr
        fcClose ((stripPrefix "</fc>" -> Just rest), to) = (rest, to)
        fcClose (x:xs, to) = fcClose (xs, x:to)

        incorrectIndicatorsErr = error $
          "Incorrect indicators: '" ++ s ++ "'"

        fcNotClosedErr = error $
          "Parse error (<fc> opened but not closed) in: '" ++ s ++ "'"

renderIndicators :: [Indicator] -> String
renderIndicators = unwords . map pairToStr

  where pairToStr (color, title) = "<fc=" ++ color ++ ">" ++ title ++ "</fc>"


getXDpyName :: Display -> String
getXDpyName = map f . displayString

  where f ':' = '_'
        f '.' = '_'
        f  x  =  x


(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixl 1 ?
