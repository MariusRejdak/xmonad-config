{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Map as M
import XMonad
import XMonad.ManageHook
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.FloatSnap
import XMonad.Actions.SpawnOn
import XMonad.Config.Kde
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import System.Process (readProcess, runInteractiveCommand, waitForProcess)
import System.IO

myTerminal          = "urxvtc"
myFocusFollowsMouse = True
myModMask           = mod4Mask
myBorderWidth       = 2
myWorkspaces        = ["1:www","2:im","3:misc"] ++ map show [4..9]
myAddWorkspaces     = ["0:video", "NSP"]
myNormalBorderColor  = "#aaaaaa"
myFocusedBorderColor = "#ff0000"
myFloatBorderColor = "#00ff00"
myCompton = "compton -b -f --backend glx --blur-background --vsync opengl --glx-use-gpushader4 -D 4 --sw-opti -e 1 -m 0.8 -G"

myConsoleScratchpads = 
    [ ((myModMask, xK_F1), "term1", "fish")
    , ((myModMask, xK_F2), "term2", "fish")
    , ((myModMask, xK_a ), "top", "htop")
    , ((myModMask, xK_s ), "mc", "mc")
    , ((myModMask, xK_d ), "mpd", "ncmpcpp")
    ]

-- key name command appName
myAppScratchpads = 
    [ ((myModMask .|. shiftMask, xK_a), "ksysguard", "ksysguard", "ksysguard")
    , ((myModMask .|. shiftMask, xK_s), "krusader", "krusader", "krusader")
    , ((myModMask .|. shiftMask, xK_d), "cantata", "cantata", "cantata")
    ]

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

scratchpads = [NS name command (appName =? thisAppName) floatingConf | (_,name,command,thisAppName) <- myAppScratchpads]
    ++ [NS name (myTerminal ++ " -name " ++ name ++ " -e " ++ command) (appName =? name) floatingConf | (_,name,command) <- myConsoleScratchpads]
    where
        floatingConf = customFloating $ W.RationalRect (1/24) (1/24) (11/12) (11/12)

myLayoutMods l = lessBorders OnlyFloat
    $ fullscreenFull
    $ desktopLayoutModifiers
    $ maximize 
    $ minimize
        l

myLayout = onWorkspace "0:video" videoLayout $ onWorkspace "2:im" imLayout $ (tile ||| mtile)
    where
        tile = myLayoutMods $ Tall nmaster delta ratio
        mtile = myLayoutMods $ Mirror $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 3/4
        delta   = 4/100
        videoLayout = noBorders Full ||| (avoidStruts . noBorders) Full
        imLayout = myLayoutMods $ reflectHoriz $ withIM (5%20) (Role "buddy_list") Grid
 
myManageHook = 
    composeOne [ isKDEOverride -?> doFloat ]
    <+> ((className =? "krunner") >>= return . not --> manageHook kde4Config)
    <+> manageSpawn
    <+> (composeAll
        [ className =? "Pidgin"             --> doShift "2:im"
        , className =? "Firefox"            --> doShift "1:www"
        , className =? "Xmessage"           --> doFloat
        , className =? "Klipper"            --> doFloat
        , className =? "Knotes"             --> doFloat
        , className =? "MPlayer"            --> doFullFloat
        , isDialog                          --> doCenterFloat
        , isKDETrayWindow                   --> doIgnore
        ] )
    <+> fullscreenManageHook
    <+> namedScratchpadManageHook scratchpads
 
myEventHook = minimizeEventHook <+> fullscreenEventHook

-- Not working :(
--runAndGetOutput :: MonadIO m => String -> m String
--runAndGetOutput cmd = liftIO $ do
--    (_, pout, _, phandle) <- runInteractiveCommand cmd
--    a <- hGetContents pout
--    waitForProcess phandle
--    return a

--checkIfRunning :: MonadIO m => String -> m Bool 
--checkIfRunning name = do
--    s <- liftIO $ runAndGetOutput $ "ps aux | grep -e '" ++ name ++ "' | grep -v grep | wc -l"
--    return ((read s :: Int) /= 0)

--doIfRunning :: MonadIO m => String -> m () -> m ()
--doIfRunning name command = do
--    b <- checkIfRunning name
--    when (not b) command

--doIfNotRunning :: MonadIO m => String -> m () -> m ()
--doIfNotRunning name command = do
--    b <- checkIfRunning name
--    when b command

--killIfRunning name = doIfRunning name $ spawn ("killall " ++ name ++ " &")
--spawnIfNotRunning name = doIfNotRunning name $ spawn (name ++ " &")

myStartupHook = do
    spawn "killall compton &"
    ewmhDesktopsStartup
    setWMName "LG3D"
    spawn "plasma-desktop"
    spawn "fishd"
    spawn "urxvtd"
    spawn "pidgin"
    spawn "kupfer --no-splash"
    spawnOn "1:www" "firefox"
    spawnOn "9" "thunderbird"
    spawn $ "sleep 2;" ++ myCompton
        
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm              , button1), \w -> focus w >> windows W.shiftMaster >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w)
    , ((modm              , button2), \w -> focus w >> snapMagicMouseResize 50 Nothing Nothing w)
    , ((modm              , button3), \w -> focus w >> Flex.mouseWindow Flex.resize w >> snapMagicMouseResize 50 (Just 50) (Just 50) w)
    , ((modm              , button4), \_ -> windows $ W.focusUp)
    , ((modm .|. shiftMask, button4), \_ -> windows $ W.swapUp)
    , ((modm              , button5), \_ -> windows $ W.focusDown)
    , ((modm .|. shiftMask, button5), \_ -> windows $ W.swapDown)
    ]

myLogHook = do
    colorBorderWhen isFloat myFloatBorderColor
    removeBorderWhen isKDEOverride
    removeBorderWhen (className =? "Klipper")

main = xmonad $ ewmh kde4Config {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces ++ myAddWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook <+> handleEventHook kde4Config,
        startupHook        = myStartupHook,
        logHook            = myLogHook
    }

    `removeKeys`
        [ (myModMask .|. shiftMask, xK_w     )
        , (myModMask .|. shiftMask, xK_e     )
        , (myModMask              , xK_q     )
        , (myModMask .|. shiftMask, xK_q     )
        , (myModMask              , xK_Tab   )
        , (myModMask              , xK_p     )
        , (myModMask              , xK_r     )
        ]

    `additionalKeys` (
        [ ((myModMask .|. shiftMask  , xK_q   ), spawn "xmonad --recompile && xmonad --restart")
        , ((myModMask                , xK_m   ), withFocused minimizeWindow)
        , ((myModMask .|. shiftMask  , xK_m   ), sendMessage RestoreNextMinimizedWin)
        , ((myModMask                , xK_f   ), withFocused (sendMessage . maximizeRestore))
        , ((myModMask                , xK_w   ), nextScreen)
        , ((myModMask                , xK_e   ), swapNextScreen)
        , ((myModMask .|. shiftMask  , xK_e   ), shiftTo Next EmptyWS)
        , ((myModMask .|. controlMask, xK_e   ), moveTo Next EmptyWS)
        , ((myModMask                , xK_Tab ), toggleWS' ["NSP"])
        , ((myModMask .|. shiftMask  , xK_f   ), spawn "firefox")
        , ((myModMask                , xK_x   ), spawn "/usr/lib/kde4/libexec/kscreenlocker_greet --immediateLock")
        , ((myModMask                , xK_r   ), spawn "kupfer")
        , ((myModMask .|. shiftMask  , xK_r   ), spawn "xprop | xmessage -file -") -- debugging stuff remove later
        , ((myModMask                , xK_0   ), windows $ W.view "0:video")
        , ((myModMask .|. shiftMask  , xK_0   ), windows $ W.shift "0:video")
        , ((myModMask .|. controlMask, xK_0   ), windows $ W.greedyView "0:video")
        ] ++ [(key, namedScratchpadAction scratchpads name) | (key,name,_) <- myConsoleScratchpads]
          ++ [(key, namedScratchpadAction scratchpads name) | (key,name,_,_) <- myAppScratchpads]
          ++ [((m .|. myModMask, k), windows $ f i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9], (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
          ++ [((controlMask .|. myModMask, k), windows $ W.greedyView i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]]
    )

    `additionalKeysP`
        [ ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioNext>", spawn "mpc next")
        , ("<XF86AudioPrev>", spawn "mpc prev")
        ]
