-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.CustomConfig
  ( Config (..)
  , getCustomConfig
  ) where

import "directory" System.Directory (getHomeDirectory)
import qualified "base" System.IO.Error as Error

import qualified "base" Control.Exception as Exception

import "base" Data.Char (toLower)

import "xmonad" XMonad (KeyMask, mod4Mask)


-- TODO implement independent workspaces
data Config =
  Config { cfgIndependentWorkspaces :: Bool
         , cfgDisplaysOrder         :: [Int]
         , cfgMetaKey               :: KeyMask
         , cfgTerminal              :: String
         , cfgTerminalDark          :: String
         , cfgTerminalLight         :: String
         , cfgAltTerminal           :: String
         , cfgAltTerminalDark       :: String
         , cfgAltTerminalLight      :: String
         , cfgFileManager           :: String
         , cfgLauncher              :: String
         , cfgBorderWidth           :: Int
         , cfgInactiveWindowOpacity :: Rational
         , cfgInactiveWindowOpacityOnlyForCurrentWs :: Bool
         } deriving Show

defaultCustomConfig :: Config
defaultCustomConfig =
  Config { cfgIndependentWorkspaces = False
         , cfgDisplaysOrder         = [1,2,3,4]
         , cfgMetaKey               = mod4Mask
         , cfgTerminal              = "gnome-terminal"
         , cfgTerminalDark          = "gnome-terminal"
         , cfgTerminalLight         = "gnome-terminal"
         , cfgAltTerminal           = "xterm"
         , cfgAltTerminalDark       = "xterm"
         , cfgAltTerminalLight      = "xterm"
         , cfgFileManager           = "nautilus"
         , cfgLauncher              = "gmrun"
         , cfgBorderWidth           = 2
         , cfgInactiveWindowOpacity = 0.7
         , cfgInactiveWindowOpacityOnlyForCurrentWs = True
         }

-- example of config.txt (all keys are optional, see defaultCustomConfig):
-- independent-workspaces = yes
-- displays-order = 3,2,1
configFile :: IO String
configFile = fmap (++ "/.xmonad/config.txt") getHomeDirectory

parseCustomConfig :: Config -> Maybe String -> Config
parseCustomConfig config configFromFile =
  case configFromFile of
    Nothing -> config
    Just _  -> resolvePairs config
             . pairs
             . _lines
             $ configFromFile
  where isSpaceSym :: Char -> Bool
        isSpaceSym x = x == ' ' || x == '\t'
        _lines :: Maybe String -> [String]
        _lines (Just a)
                     | a == ""   = []
                     | otherwise = foldr reducer [""] a
                     where reducer symbol (x:xs)
                             | symbol == '\n' = "":x:xs
                             | otherwise      = (symbol:x):xs
                           reducer _ _ = error "unexpected value"
        _lines _ = error "unexpected value"
        pairs :: [String] -> [(String, String)]
        pairs = map checkForAvailableKey
              . foldr splitReducer []
              . filter (/= "")
              . map clearSidesSpaces
          where clearSidesSpaces = reverse
                                 . dropWhile isSpaceSym
                                 . reverse
                                 . dropWhile isSpaceSym
                splitReducer line acc = (k, v) : acc
                  where k = clearSidesSpaces $ takeWhile (/= '=') line
                        v = clearSidesSpaces $ tail $ dropWhile (/= '=') line
                checkForAvailableKey (k, v)
                  | k `elem` availableKeys = (k, v)
                  | otherwise = error "Unknown config key"
                  where availableKeys = [ "independent-workspaces"
                                        , "displays-order"
                                        , "terminal"
                                        , "terminal-dark"
                                        , "terminal-light"
                                        , "alternative-terminal"
                                        , "alternative-terminal-dark"
                                        , "alternative-terminal-light"
                                        , "file-manager"
                                        , "launcher"
                                        , "border-width"
                                        , "inactive-window-opacity"
                                        , "inactive-window-opacity-\
                                          \only-for-current-workspace"
                                        ]
        resolvePairs :: Config -> [(String, String)] -> Config
        resolvePairs cfg [] = cfg
        resolvePairs cfg ((k, v):_pairs) = resolvePairs newConfig _pairs
          where unexpectedValueError =
                  error $ "Unexpected value of '" ++ k ++ "' in config"

                boolKey :: String -> Bool
                boolKey x = case map toLower x of
                                 "yes" -> True
                                 "no"  -> False
                                 _     -> unexpectedValueError

                newConfig = case k of

                  "independent-workspaces" ->
                     cfg { cfgIndependentWorkspaces = boolKey v }

                  "displays-order" ->
                    let cleanStr = filter (not . isSpaceSym) v
                        isValidSym x = x `elem` ',':['0'..'9']
                        validStr
                          | all isValidSym cleanStr = cleanStr
                          | otherwise = unexpectedValueError
                        listReducer :: Char -> [String] -> [String]
                        listReducer ',' acc = "":acc
                        listReducer c (x:xs) = (c:x):xs
                        listReducer _ _ = error "unexpected value"
                        order
                          | length result >= 2 = result
                          | otherwise = error $ "'" ++ k ++ "' config value\
                                          \ should have at least 2 items"
                          where result = map read
                                       $ foldr listReducer [""] validStr
                    in cfg { cfgDisplaysOrder = order }

                  "terminal" ->
                    cfg { cfgTerminal = v }
                  "terminal-dark" ->
                    cfg { cfgTerminalDark = v }
                  "terminal-light" ->
                    cfg { cfgTerminalLight = v }
                  "alternative-terminal" ->
                    cfg { cfgAltTerminal = v }
                  "alternative-terminal-dark" ->
                    cfg { cfgAltTerminalDark = v }
                  "alternative-terminal-light" ->
                    cfg { cfgAltTerminalLight = v }
                  "file-manager" ->
                    cfg { cfgFileManager = v }
                  "launcher" ->
                    cfg { cfgLauncher = v }
                  "border-width" ->
                    cfg { cfgBorderWidth = read v }

                  "inactive-window-opacity" ->
                    cfg { cfgInactiveWindowOpacity =
                              toRational (read v :: Float) }
                  "inactive-window-opacity-only-for-current-workspace" ->
                    cfg { cfgInactiveWindowOpacityOnlyForCurrentWs =
                              boolKey v }

                  _ -> error "unexpected value"

getCustomConfig :: IO Config
getCustomConfig = parseCustomConfig defaultCustomConfig <$> readConfigFile
  where readConfigFile = do
          filePath <- configFile
          Exception.catch (Just <$> readFile filePath) handleExists
        handleExists e
          | Error.isDoesNotExistError e = return Nothing
          | otherwise = Exception.throwIO e
