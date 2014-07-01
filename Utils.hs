{-# LANGUAGE FlexibleContexts #-}

module Utils where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isInProperty, isFullscreen)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.WorkspaceCompare (getSortByIndex, WorkspaceSort, getSortByXineramaRule)

import Data.Monoid

import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import System.Process (readProcess, runInteractiveCommand, waitForProcess)
import System.IO
import Codec.Binary.UTF8.String
import Codec.Binary.Base64.String as Base64


doSink :: Query (Endo WindowSet)
doSink = ask >>= doF . W.sink

-- | Set the border color when the query is satisfied.  Should be added to the
--   ManageHook.
colorBorderWhen :: Query Bool -> String -> X ()
colorBorderWhen q cl = withFocused $ \w -> runQuery q w >>= flip when (setWindowBorder' cl w)

-- | Set the border color when the query is satisfied.  Should be added to the
--   ManageHook.
removeBorderWhen :: Query Bool -> X ()
removeBorderWhen q = withFocused $ \w -> runQuery q w >>= flip when (removeWindowBorder' w)

-- | Give set the border color of a window to the given HTML color code.
setWindowBorder' ::(MonadReader XConf m, MonadIO m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

removeWindowBorder' ::(MonadReader XConf m, MonadIO m) => Window -> m ()
removeWindowBorder' w = do
    XConf { display = d } <- ask
    io $ setWindowBorder d w 0

-- | Is the focused window a floating window?
isFloat :: Query Bool
isFloat = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ M.member w $ W.floating ws)

-- | Is a KDE floating window?
isKDEOverride :: Query Bool
isKDEOverride = do
    isover <- isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    isfs <- isFullscreen
    return $! isover && (not isfs)

-- | Switch to next hidden workspace
switchWsNoSP :: [WorkspaceId] -> Int -> X ()
switchWsNoSP list d = do
    ws <- findWorkspace (getSortByIndexNoSP list) Next HiddenWS d
    windows . W.view $ ws

-- | Shift window to next hidden workspace
shiftWsNoSP :: [WorkspaceId] -> Int -> X ()
shiftWsNoSP list d = do
    ws <- findWorkspace (getSortByIndexNoSP list) Next HiddenWS d
    windows . W.shift $ ws

-- | Shift window to workspace displayed previously
shiftToggleWS' :: [WorkspaceId] -> X ()
shiftToggleWS' skips = do
    hs' <- cleanHiddens skips
    unless (null hs') (windows . W.shift . W.tag $ head hs')
    where
        -- | Copied from XMonad.Actions.CycleWS
        cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
        cleanHiddens skips =  gets $ (flip skipTags) skips . W.hidden . windowset

viewNextWS :: [WorkspaceId] -> X ()
viewNextWS l = switchWsNoSP l 1

viewPrevWS :: [WorkspaceId] -> X ()
viewPrevWS l = switchWsNoSP l (-1)

shiftNextWS :: [WorkspaceId] -> X ()
shiftNextWS l = shiftWsNoSP l 1

shiftPrevWS :: [WorkspaceId] -> X ()
shiftPrevWS l = shiftWsNoSP l (-1)

-- | Get workspaces without listed ones
getSortByIndexNoSP :: [WorkspaceId] -> X WorkspaceSort
getSortByIndexNoSP list = fmap (. filterOutWorkspaceList list) getSortByIndex
    where
        filterOutWorkspaceList list = filter (\(W.Workspace tag _ _) -> not $ tag `elem` list)

-- | Dynamic log for xmonad-log-plasmoid
myDynamicLog :: X ()
myDynamicLog = do
    dynamicLogString (namedScratchpadFilterOutWorkspacePP myPP) >>= \w -> spawn $ "dbus-send --type=\"method_call\" --dest=org.xmonad.LogService /Log org.xmonad.Log.msg string:\""++(Base64.encode w)++"\""

-- | Pretty printer for myDynamicLog
myPP :: PP
myPP = PP { ppCurrent         = wrap "[[c]]" ""
          , ppVisible         = wrap "[[v]]" ""
          , ppHidden          = id
          , ppHiddenNoWindows = const ""
          , ppUrgent          = wrap "[[u]]" ""
          , ppSep             = "[[|]]"
          , ppWsSep           = "[[|]]"
          , ppTitle           = shorten 160
          , ppTitleSanitize   = wrap "[[t]]" ""
          , ppLayout          = wrap "[[l]]" ""
          , ppOrder           = id
          , ppOutput          = putStrLn
          , ppSort            = getSortByXineramaRule
          , ppExtras          = []
        }
