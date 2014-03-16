import XMonad
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
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders hiding (Never)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare (getSortByIndex, getSortByXineramaRule)

import Data.Ratio ((%))
import Data.Monoid (mconcat)
import qualified Data.Map as M

import Codec.Binary.UTF8.String
import Codec.Binary.Base64.String as Base64

import Utils

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
    , ((myModMask, xK_F3), "term3", "fish")
    , ((myModMask, xK_F4), "term4", "fish")
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

scratchpads = [NS name command (appName =? thisAppName) floatingConf | (_,name,command,thisAppName) <- myAppScratchpads]
    ++ [NS name (myTerminal ++ " -name " ++ name ++ " -e " ++ command) (appName =? name) floatingConf | (_,name,command) <- myConsoleScratchpads]
    where
        floatingConf = customFloating $ W.RationalRect (1/24) (1/24) (11/12) (11/12)

myLayoutMods l = lessBorders OnlyFloat
    $ fullscreenFull
    $ desktopLayoutModifiers
    $ boringWindows
    $ maximize
    $ minimize
        l

myLayout = onWorkspace "0:video" videoLayout $ onWorkspace "2:im" imLayout $ (tiledR ||| tiledL ||| tiledB)
    where
        tiledR = named "Tiled right" $ myLayoutMods $ Tall nmaster delta ratio
        tiledL = named "Tiled left" $ myLayoutMods $ reflectHoriz $ Tall nmaster delta ratio
        tiledB = named "Tiled bottom" $ myLayoutMods $ Mirror $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 3/4
        delta   = 4/100
        videoLayout = noBorders Full ||| (avoidStruts . noBorders) Full
        imLayout = named "IM Grid" $ myLayoutMods $ reflectHoriz $ withIM (5%20) (Role "buddy_list") Grid

myManageHook =
    composeOne [ isKDEOverride -?> doFloat ]
    <+> ((className =? "krunner") >>= return . not --> manageHook kde4Config)
    <+> manageSpawn
    <+> (composeAll
        [ className =? "Pidgin"             --> doShift "2:im"
        , className =? "Firefox"            --> doShift "1:www"
        , className =? "Chromium"            --> doShift "1:www"
        , className =? "Xmessage"           --> doFloat
        , className =? "Klipper"            --> doFloat
        , className =? "Knotes"             --> doFloat
        , className =? "Smplayer"           --> doShift "0:video" <+> doSink
        , className =? "Vlc"                --> doShift "0:video" <+> doSink
        , className =? "Steam"              --> doShift "0:video" <+> doSink
        , className =? "MPlayer"            --> doFullFloat
        , className =? "Sm"                 --> doFullFloat
        , isDialog                          --> doCenterFloat
        , isKDETrayWindow                   --> doIgnore
        ] )
    <+> fullscreenManageHook
    <+> namedScratchpadManageHook scratchpads

myEventHook = mconcat [minimizeEventHook, fullscreenEventHook]

myStartupHook = do
    spawn "killall compton &"
    ewmhDesktopsStartup
    setWMName "LG3D"
    spawn "plasma-desktop"
    spawn "fishd"
    spawn "urxvtd"
    spawn "pidgin"
    spawn "kupfer --no-splash"
    --spawnOn "1:www" "chromium"
    spawnOn "9" "thunderbird"
    spawn $ "sleep 2;" ++ myCompton

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm              , button1), \w -> focus w >> windows W.shiftMaster >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w)
    , ((modm              , button2), \w -> focus w >> snapMagicMouseResize 50 Nothing Nothing w)
    , ((modm              , button3), \w -> focus w >> Flex.mouseWindow Flex.resize w >> snapMagicMouseResize 50 (Just 50) (Just 50) w)
    , ((modm              , button4), \_ -> focusUp)
    , ((modm .|. shiftMask, button4), \_ -> windows $ W.swapUp)
    , ((modm              , button5), \_ -> focusDown)
    , ((modm .|. shiftMask, button5), \_ -> windows $ W.swapDown)
    ]

myDynamicLog :: X ()
myDynamicLog = do
    dynamicLogString (namedScratchpadFilterOutWorkspacePP myPP) >>= \w -> spawn $ "dbus-send --type=\"method_call\" --dest=org.xmonad.LogService /Log org.xmonad.Log.msg string:\""++(Base64.encode w)++"\""

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

myLogHook = do
    colorBorderWhen isFloat myFloatBorderColor
    removeBorderWhen isKDEOverride
    removeBorderWhen (className =? "Klipper")
    removeBorderWhen (className =? "Kupfer.py")
    myDynamicLog

main = xmonad $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "#ff00ff" } urgencyConfig { suppressWhen = Focused } $ ewmh kde4Config {
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
        [ (myModMask              , xK_q     )
        , (myModMask              , xK_Tab   )
        , (myModMask              , xK_p     )
        ]

    `additionalKeys` (
        [ ((myModMask .|. shiftMask  , xK_q   ), spawn "xmonad --recompile && xmonad --restart")
        , ((myModMask                , xK_m   ), withFocused minimizeWindow)
        , ((myModMask .|. shiftMask  , xK_m   ), sendMessage RestoreNextMinimizedWin)
        , ((myModMask                , xK_f   ), withFocused (sendMessage . maximizeRestore))
        , ((myModMask                , xK_w   ), nextScreen)
        , ((myModMask                , xK_e   ), swapNextScreen)
        , ((myModMask                , xK_j   ), focusUp)
        , ((myModMask                , xK_k   ), focusDown)
        , ((myModMask                , xK_z   ), focusMaster)
        , ((myModMask                , xK_Tab ), toggleWS' ["NSP"])
        , ((myModMask .|. shiftMask  , xK_f   ), spawn "chromium")
        , ((myModMask                , xK_x   ), spawn "/usr/lib/kde4/libexec/kscreenlocker_greet --immediateLock")
        , ((myModMask                , xK_r   ), spawn "kupfer")
        , ((myModMask .|. shiftMask  , xK_r   ), spawn "xprop | xmessage -file -") -- debugging stuff remove later
        , ((myModMask .|. shiftMask  , xK_i   ), spawn "sm -f white -b black \"\"")
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
