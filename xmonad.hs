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
import XMonad.Util.WindowProperties (getProp32s)

myTerminal          = "urxvtc"
myFocusFollowsMouse = True
myModMask           = mod4Mask
myBorderWidth       = 2
myWorkspaces        = ["1:www","2:im","3:misc"] ++ map show [4..9]
myNormalBorderColor  = "#aaaaaa"
myFocusedBorderColor = "#ff0000"
myCompton = "sleep 2; compton -b -f --backend glx --blur-background --vsync opengl --glx-use-gpushader4 -D 4 --sw-opti -e 0.7 -m 0.8 -G"

myConsoleScratchpads = 
    [ (xK_F1, "term1", "fish")
    , (xK_F2, "term2", "fish")
    , (xK_a, "top", "htop")
    , (xK_d, "mpd", "ncmpcpp")
    ]

doUnFloat = ask >>= doF . W.sink

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

-- doRemoveBorders :: Query (Endo WindowSet)
-- doRemoveBorders = ask >>= \w -> liftX . withDisplay $ \d -> io $ setWindowBorder d w 0

isKDEOverride = do
    isover <- isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    isfs <- isFullscreen
    return $! isover && (not isfs)

-- TODO: more meta scratchpads like consoles
scratchpads = [NS "choqok" "choqok" (appName =? "choqok") floatingConf]
    ++ [NS "krusader" "krusader" (appName =? "krusader") floatingConf]
    ++ [NS name (myTerminal ++ " -name " ++ name ++ " -e " ++ command) (appName =? name) floatingConf | (_,name,command) <- myConsoleScratchpads]
        where
            floatingConf = customFloating $ W.RationalRect (1/24) (1/24) (11/12) (11/12)

myLayoutMods l = fullscreenFull
    $ desktopLayoutModifiers 
    $ lessBorders OnlyFloat
    $ maximize 
    $ minimize
        l

myLayout = onWorkspace "2:im" imLayout $ (tile ||| mtile)
    where
        tile = myLayoutMods $ Tall nmaster delta ratio
        mtile = myLayoutMods $ Mirror $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 3/4
        delta   = 4/100
        imLayout = myLayoutMods $ reflectHoriz $ withIM (5%20) (Role "buddy_list") Grid
 
myManageHook = 
    composeOne [ isKDEOverride -?> doFloat ]
    <+> ((className =? "krunner") >>= return . not --> manageHook kde4Config)
    -- <+> (kdeOverride --> doFloat)
    <+> (composeAll
        [ className =? "Pidgin"             --> doShift "2:im"
        , className =? "Firefox"            --> doShift "1:www"
        , className =? "Xmessage"           --> doFloat
        --, className =? "plasma"             --> doFloat
        --, className =? "Plasma"             --> doFloat
        --, className =? "plasma-desktop"     --> doFloat
        --, className =? "Plasma-desktop"     --> doFloat
        --, className =? "krunner"            --> doFloat
        --, className =? "Klipper"            --> doFloat
        , className =? "Knotes"             --> doFloat
        --, appName   =? "desktop_window"     --> doIgnore
        --, appName   =? "kdesktop"           --> doIgnore
        , isDialog                          --> doCenterFloat
        --, isKDETrayWindow                   --> doIgnore
        ] )
    -- <+> fullscreenManageHook
    <+> namedScratchpadManageHook scratchpads
 
myEventHook = minimizeEventHook <+> fullscreenEventHook

myStartupHook = do
    -- spawn "if [ $(ps aux | grep -e 'compton$' | grep -v grep | wc -l | tr -s \"\n\") -eq 0 ]; then killall compton; fi"
    ewmhDesktopsStartup
    setWMName "LG3D"
    let ifRunningWrap command = "if [ $(ps aux | grep -e '" ++ command ++ "$' | grep -v grep | wc -l | tr -s \"\n\") -eq 0 ]; then " ++ command ++ "; fi"
    spawn $ ifRunningWrap "urxvtd"
    spawn $ ifRunningWrap "pidgin"
    -- spawn myCompton
    spawn "fishd"
        
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               button1), \w -> focus w >> windows W.shiftMaster >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w)
    , ((modm,               button2), \w -> focus w >> snapMagicMouseResize 50 Nothing Nothing w)
    , ((modm,               button3), \w -> focus w >> Flex.mouseWindow Flex.resize w >> snapMagicMouseResize 50 (Just 50) (Just 50) w)
    , ((modm,               button4), \_ -> windows $ W.swapUp)
    , ((modm,               button5), \_ -> windows $ W.swapDown)
    ]

main = xmonad $ ewmh kde4Config {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook <+> handleEventHook kde4Config,
        startupHook        = myStartupHook
    }

    `removeKeys`
        [ (myModMask .|. shiftMask, xK_w     )
        , (myModMask .|. shiftMask, xK_e     )
        , (myModMask .|. shiftMask, xK_q     )
        , (myModMask              , xK_Tab   )
        , (myModMask              , xK_p     )
        ]

    `additionalKeys` (
        [ ((myModMask              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
        , ((myModMask              , xK_m     ), withFocused minimizeWindow)
        , ((myModMask .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
        , ((myModMask              , xK_f     ), withFocused (sendMessage . maximizeRestore))
        , ((myModMask              , xK_w     ), nextScreen)
        , ((myModMask .|. shiftMask, xK_w     ), swapNextScreen)
        , ((myModMask .|. shiftMask, xK_e     ), shiftTo Next EmptyWS)
        , ((myModMask              , xK_e     ), moveTo Next EmptyWS)
        , ((myModMask              , xK_Tab   ), toggleWS' ["NSP"])
        , ((myModMask              , xK_b     ), spawn "firefox")
        , ((myModMask              , xK_x     ), spawn "/usr/lib/kde4/libexec/kscreenlocker_greet --immediateLock")
        , ((myModMask              , xK_p     ), spawn "kupfer")
        , ((myModMask              , xK_r), spawn "xprop > ~/test.txt") -- debugging stuff remove later
        , ((myModMask              , xK_F3), namedScratchpadAction scratchpads "choqok")
        , ((myModMask              , xK_s), namedScratchpadAction scratchpads "krusader")
        ] ++ [((myModMask, key), namedScratchpadAction scratchpads name) | (key,name,_) <- myConsoleScratchpads]
          ++ [((m .|. myModMask, k), windows $ f i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9], (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
          ++ [((controlMask .|. myModMask, k), windows $ W.greedyView i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]]
    )

    `additionalKeysP`
        [ ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioNext>", spawn "mpc next")
        , ("<XF86AudioPrev>", spawn "mpc prev")
        ]
