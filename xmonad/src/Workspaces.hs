-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xmonadrc/master/LICENSE

module Workspaces
  ( myWorkspacesBareList
  , myWorkspaces
  ) where

-- local imports

import Utils (xmobarEscape)


myWorkspacesBareList :: [String]
myWorkspacesBareList = map show ([1..9] :: [Int])

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ myWorkspacesBareList
  where
    clickable l = [ "<action=xdotool key super+" ++ k ++ ">" ++ ws ++ "</action>"
                  | (k, ws) <- zip myWorkspacesKeysList l ]
    myWorkspacesKeysList = map numpadHackMap [1..length myWorkspacesBareList]
    numpadHackMap x = case x of
                           0 -> "KP_Insert"
                           1 -> "KP_End"
                           2 -> "KP_Down"
                           3 -> "KP_Next"
                           4 -> "KP_Left"
                           5 -> "KP_Begin"
                           6 -> "KP_Right"
                           7 -> "KP_Home"
                           8 -> "KP_Up"
                           9 -> "KP_Prior"
                           _ -> error "unexpected value"
