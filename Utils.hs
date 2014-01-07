{-# LANGUAGE FlexibleContexts #-}

module Utils where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import Data.Monoid

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import System.Process (readProcess, runInteractiveCommand, waitForProcess)
import System.IO


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