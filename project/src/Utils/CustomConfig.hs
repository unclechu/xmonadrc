-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Utils.CustomConfig
  ( Config(..)
  , getCustomConfig
  ) where

import System.Directory (getHomeDirectory)
import qualified System.IO.Error as Error

import qualified Control.Exception as Exception

import Data.Char (toLower)

import XMonad (KeyMask, mod4Mask)


-- TODO implement independent workspaces
data Config =
  Config { cfgIndependentWorkspaces :: Bool
         , cfgDisplaysOrder         :: [Int]
         , cfgMetaKey               :: KeyMask
         , cfgTerminal              :: String
         , cfgTerminalDark          :: String
         , cfgTerminalLight         :: String
         , cfgFileManager           :: String
         , cfgLauncher              :: String
         , cfgInactiveWindowOpacity :: Rational
         } deriving Show

defaultCustomConfig =
  Config { cfgIndependentWorkspaces = False
         , cfgDisplaysOrder         = [1,2,3]
         , cfgMetaKey               = mod4Mask
         , cfgTerminal              = "terminator"
         , cfgTerminalDark          = "terminator --profile dark"
         , cfgTerminalLight         = "terminator --profile light"
         , cfgFileManager           = "nautilus"
         , cfgLauncher              = "gmrun"
         , cfgInactiveWindowOpacity = 0.9
         }

-- example of config.txt (all keys are optional, see defaultCustomConfig):
-- independent-workspaces = yes
-- displays-order = 3,2,1
configFile :: IO String
configFile = fmap (++ "/.xmonad/config.txt") getHomeDirectory

parseCustomConfig config configFromFile =
  case configFromFile of
    Nothing -> config
    Just x  -> resolvePairs config
             . pairs
             . lines
             $ configFromFile
  where isSpaceSym :: Char -> Bool
        isSpaceSym x = x == ' ' || x == '\t'
        lines :: Maybe String -> [String]
        lines (Just x)
                    | x == ""   = []
                    | otherwise = foldr reducer [""] x
                    where reducer symbol (x:xs)
                            | symbol == '\n' = "":x:xs
                            | otherwise      = (symbol:x):xs
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
                                        , "file-manager"
                                        , "launcher"
                                        , "inactive-window-opacity"
                                        ]
        resolvePairs :: Config -> [(String, String)] -> Config
        resolvePairs config [] = config
        resolvePairs config ((k, v):pairs) = resolvePairs newConfig pairs
          where newConfig = case k of
                  "independent-workspaces" ->
                    let nv = map toLower v
                    in case nv of
                         "yes" -> config { cfgIndependentWorkspaces = True  }
                         "no"  -> config { cfgIndependentWorkspaces = False }
                         _ -> error "Unexpected value of\
                                \ independent-workspaces in config"
                  "displays-order" ->
                    let cleanStr = filter (not . isSpaceSym) v
                        isValidSym x = x `elem` ',':['0'..'9']
                        validStr
                          | all isValidSym cleanStr = cleanStr
                          | otherwise = error "Unexpected value of\
                                          \ displays-order in config"
                        listReducer :: Char -> [String] -> [String]
                        listReducer ',' acc = "":acc
                        listReducer c (x:xs) = (c:x):xs
                        order
                          | length result >= 2 = result
                          | otherwise = error "displays-order should have\
                                          \ at least 2 items"
                          where result = map read
                                       $ foldr listReducer [""] validStr
                    in config { cfgDisplaysOrder = order }
                  "terminal"                -> config { cfgTerminal      = v }
                  "terminal-dark"           -> config { cfgTerminalDark  = v }
                  "terminal-light"          -> config { cfgTerminalLight = v }
                  "file-manager"            -> config { cfgFileManager   = v }
                  "launcher"                -> config { cfgLauncher      = v }
                  "inactive-window-opacity" ->
                    config { cfgInactiveWindowOpacity =
                              toRational (read v :: Float) }

getCustomConfig :: IO Config
getCustomConfig = parseCustomConfig defaultCustomConfig <$> readConfigFile
  where readConfigFile = do
          filePath <- configFile
          Exception.catch (Just <$> readFile filePath) handleExists
        handleExists e
          | Error.isDoesNotExistError e = return Nothing
          | otherwise = Exception.throwIO e
