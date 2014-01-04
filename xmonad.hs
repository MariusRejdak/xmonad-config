import System.IO
import qualified System.IO.UTF8
import Control.Monad (when)
import System.Environment (getArgs)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
import XMonad.Util.Replace
import Data.Ratio ((%))
import qualified XMonad.StackSet as S
import XMonad.Util.EZConfig
import XMonad.Hooks.ICCCMFocus
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

--------------------------------
--		STUFF
--------------------------------

withSpawnedPipe cmd f = do
    pipe <- spawnPipe cmd
    f pipe
    hClose pipe

myRubyHack = "/home/hake5/.xmonad/struts.rb"

main = do
    --dzenTitleBarPipe  <- spawnPipe myDzenTitleBar
    --withSpawnedPipe myDzenTitleBar $ \ dzenTitleBarPipe -> do
    --dzenStatusBarPipe <- spawnPipe myDzenStatusBar
    --withSpawnedPipe myDzenStatusBar $ \ dzenStatusBarPipe -> do
    --dzenMusicBarPipe  <- spawnPipe myDzenMusicBar
    --withSpawnedPipe myDzenMusicBar $ \ dzenMusicBarPipe -> do
    --spawn myTray
    args <- getArgs
    when ("--replace" `elem` args) replace
    withSpawnedPipe myRubyHack $ \ dzenTitleBarPipe -> do

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal 				= myTerminal
        , modMask  				= myModMask
        , borderWidth			= myBorderWidth
        , startupHook 			= myStartupHook
        , layoutHook 			= myLayout
        , logHook 				= myLogHook dzenTitleBarPipe
        , manageHook 			= myManageHook <+> manageHook defaultConfig
        , normalBorderColor 	= myNormalBorderColor
        , focusedBorderColor 	= myFocusedBorderColor
        , focusFollowsMouse		= myFocusFollowsMouse
        , workspaces			= myWorkspaces
        , handleEventHook			= fullscreenEventHook
        }

        `removeKeys`
            [(myModMask, xK_p)] -- ++
            --[((m .|. myModMask, k), windows $ f i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]]

        `additionalKeys`
            ([((myModMask, xK_p), shellPrompt myXPConfig)] ++
             [((myModMask, xK_z), spawn "xscreensaver-command -lock & xset dpms force off")] ++
             [((myModMask, xK_z), spawn "sudo pm-suspend")] ++
             [((myModMask, xK_b), spawn "firefox")] ++
             [((myModMask, xK_o), restart "/home/hake5/ob" True)] ++
             [((myModMask .|. shiftMask, xK_F12), spawn "sudo pm-hibernate")] ++
                [((m .|. myModMask, k), windows $ f i)
                | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
                  (f, m) <- [(S.view, 0), (S.shift, shiftMask)]] ++
            [((controlMask .|. myModMask, k), windows $ S.greedyView i)
                | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]])

	`additionalKeysP`
	    (
	    [ ("<XF86Sleep>", spawn "xscreensaver-command -lock & sudo pm-suspend")
	    , ("<XF86PowerDown>", spawn "sudo pm-hibernate")
	    , ("<XF86ScreenSaver>", spawn "xset dpms force off")
	    , ("<XF86AudioPlay>", spawn "mpc toggle")
	    , ("<XF86AudioStop>", spawn "mpc stop")
	    , ("<XF86AudioNext>", spawn "mpc next")
	    , ("<XF86AudioPrev>", spawn "mpc prev")
	    , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 -q sset Master 1+ unmute")
	    , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 -q sset Master 1- unmute")
	    , ("<XF86AudioMute>", spawn "amixer -c 0 sset Master toggle")

	    ])

---------------------------------
--		CONSTANTS
---------------------------------i

myStartupHook = do
    takeTopFocus
    --spawn "urxvt -T weechat -e weechat-curses" -- --> doShift "1:irc"
    spawn "urxvt -T ncmpcpp -e ncmpcpp" -- --> doShift "9:mp3"
    --spawn "firefox"
    spawn "pidgin"
    spawn "urxvt -T URxvtConsole"
    setWMName "LG3D"

myTerminal		 		= "urxvt"
myModMask 		 		= mod4Mask
myBorderWidth	 		= 2
myWorkspaces 			= ["1:irc", "2:im", "3:www", "4:dev1", "5:dev2", "6:doc", "7:misc", "8:vid", "9:mp3"]
myFocusFollowsMouse 	= True
myFont					= "-*-dejavu sans mono-medium-r-*-*-10-*-*-*-*-*-iso8859-*"
myImgDirectory			= ".dzen/bitmaps/"

---------------------------------
--		COLORS
--------------------------------

myBarColor				  = "#111111"
myBarTextColor			  = "#babdb6"
myBarInactiveTextColor    = "#555753"
myNormalBorderColor 	  = "#555753"
myFocusedBorderColor 	  = "#a40000"
myCurrentWorkspaceColor   = "#73d216"
myDefaultBarColor 		  = "#555753"
myUrgentWorkspaceColor    = "#edd400"

myXPConfig = defaultXPConfig
    {
		font = myFont,
		fgColor = myBarInactiveTextColor,
		bgColor = myBarColor,
		bgHLight    = myBarColor,
		fgHLight    = myBarTextColor,
		position = Top,
		historySize = 512,
		showCompletionOnTab = True,
		historyFilter = deleteConsecutive,
		promptBorderWidth = 0,
		height = 16
	}

--myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full) ||| Full
myLayout = onWorkspace "8:vid" videoLayout $ smartBorders (onWorkspace "2:im" imLayout $ standardLayouts)

	where
		standardLayouts = avoidStruts(tiled ||| Mirror tiled ||| Full) ||| noBorders Full
			where
    			tiled   = Tall nmaster delta ratio
    			nmaster = 1
    			ratio   = 3/4
    			delta   = 3/100

		videoLayout = noBorders Full ||| avoidStruts(noBorders Full)

		--imLayout = avoidStruts(reflectHoriz $ withIM (5%20) (Title ("Kadu: " ++ myGGNum)) Grid)
		imLayout = avoidStruts(smartBorders (reflectHoriz $ withIM (6%20) (Role "buddy_list") Grid))

myLogHook pipe = dynamicLogWithPP $ myDzenPP pipe

-- | Move the window to the floating layer.
--doSink :: ManageHook
doSink = ask >>= \w -> doF (S.sink w)

myManageHook = composeAll
		[isFullscreen --> doFullFloat,
		 isDialog --> doCenterFloat,
		 className =? "trayer" --> doIgnore,
		 className =? "Xfce4-notifyd" --> doIgnore,
         className =? "Conky" --> doIgnore,
		 title =? "weechat" --> doShift "2:im",
		 title =? "URxvtConsole" --> doShift "7:misc",
		 className =? "Chromium" --> doShift "3:www",
		 className =? "luakit" --> doShift "3:www",
		 className =? "Firefox" --> doShift "3:www",
		 className =? "Pidgin" --> doShift "2:im",
		 --className =? "Evince" --> doShift "6:doc",
		 className =? "Pcmanfm" --> doShift "7:misc",
		 className =? "Thunar" --> doShift "7:misc",
		 className =? "MPlayer" --> doShift "8:vid" <+> doSink,
         	 className =? "Smplayer" --> doShift "8:vid" <+> doSink,
		 title =? "ncmpcpp" --> doShift "9:mp3"]
	where
		ignore  = ["trayer"]

myDzenTitleBar   = "dzen2 -w 1206 -h 16 -ta l -bg \"" ++ myBarColor ++ "\" -fg \"" ++ myBarInactiveTextColor  ++ "\" -fn \"" ++ myFont  ++ "\""
myDzenStatusBar  = myTopBar ++  " | dzen2 -y 1184 -x 600 -h 16 -ta r -bg \"" ++ myBarColor ++ "\" -fn \"" ++ myFont ++ "\""
myDzenMusicBar   = myMusicBar ++ " | dzen2 -y 1184 -h 16 -x 0 -w 600 -ta l -bg \"" ++ myBarColor ++ "\" -fn \"" ++ myFont ++ "\""
myTray	         = "trayer --monitor 1 --edge top --align right --heighttype pixel --height 16 --widthtype pixels --width 160 --transparent true --alpha 0 --tint 0x111111"
myWirelessApplet = "nm-applet"

myTopBar = "conky -c ~/.conky_bar_top"
myMusicBar = "conky -c ~/.conky_bar_bottom"

myDzenPP h = defaultPP
	{
	ppOutput  = System.IO.hPutStrLn h, --UTF8.hPutStrLn h,
	ppCurrent = (\x -> wrapFgColor myCurrentWorkspaceColor ++ wrapImg "corner.xbm" ++ x ++ " ^fg()^bg()"),
	ppHidden  = (\x -> "^ca(1, xdotool key Super+" ++ [x!!0]  ++ ")" ++ wrapImg "corner.xbm" ++ x ++ " ^ca()^fg()^bg()"), --(\x -> wrapFgColor myPopulatedWorkspaceColor ++ wrapImg "corner.xbm" ++ x ++ "^fg()"),
	ppUrgent  = (\x -> wrapBgColor myUrgentWorkspaceColor ++ wrapFgColor "#000000" ++ "^ca(1, xdotool key Super+" ++ [x!!0] ++ ")" ++ wrapImg "corner.xbm"  ++ x ++ " ^ca()^fg()^bg()" ),
	ppSep     = wrapFgColor myBarTextColor ++ " | ", --hack, so that rest is proper color
	ppWsSep   = "",
	ppTitle   = (\x -> x)
	}
	where
		wrapFgColor color    = "^fg(" ++ color ++ ")"
		wrapBgColor color    = "^bg(" ++ color ++ ")"
		wrapImg img 	     = "^i(" ++ myImgDirectory ++ img ++ ")"
		wrapImgCol img color = "^fg(" ++ color ++ ")^i(" ++ myImgDirectory ++ img ++ ")"
