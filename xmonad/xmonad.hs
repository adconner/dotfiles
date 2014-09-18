{-# LANGUAGE DeriveDataTypeable #-}

import System.IO
import System.IO.Unsafe(unsafePerformIO)
import System.Environment(getEnvironment)
import System.Exit

import XMonad hiding ((|||))

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.LayoutCombinators -- for JumpToLayout
import XMonad.Layout.Fullscreen
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders

import XMonad.Actions.CycleRecentWS(cycleRecentWS)
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.Promote
-- import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace

import XMonad.Util.Run(spawnPipe)

import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- for mouse cursor
-- import Graphics.X11.Xlib
-- import Graphics.X11.Xlib.Extras

myTerminal           = envVarDefault "XTERM" "xterm"
myShell              = envVarDefault "SHELL" "bash"
myBorderWidth        = 1
myModMask            = mod4Mask
myWorkspaces         = ["1:gen","2:trash","3","4","5","6","7","8","9","0"]
myNormalBorderColor  = "gray" -- "#dddddd"
myFocusedBorderColor = "red" -- "#ff0000"
myAddNice            = 10 -- keep xmonad at higher priority than other interactive programs

------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), mySpawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), mySpawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), mySpawn "dmenu_term_run")
    , ((modm,               xK_d     ), kill)
    -- , ((modm,               xK_space ), sendMessage NextLayout)
    -- , ((modm,               xK_space ), cycleThroughLayouts ["Tall", "Full", "Mirror Tall"])
    , ((modm,               xK_space ), cycleThroughLayouts ["Tall", "TwoPane"])
    , ((modm .|. shiftMask, xK_f     ), sendMessage $ JumpToLayout "Full")
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_m     ), windows . W.shift $ myWorkspaces !! 1 )
    , ((modm,               xK_v     ), viewEmptyWorkspace )
    , ((modm .|. shiftMask, xK_v     ), tagToEmptyWorkspace )
    -- , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm,               xK_Return), promote)
    -- , ((modm,               xK_Return), dwmpromote)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_s     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_period), sendMessage (IncMasterN 1))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), mySpawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_semicolon), toggleMouse)
    -- , ((modm              , xK_apostrophe), mySpawn "xdotool getwindowfocus click --window %1 1")
    --   --  needed for firefox, seems finnicky
     
    , ((mod1Mask          , xK_Tab   ), cycleRecentWS [xK_Alt_L] xK_Tab xK_apostrophe)

      -- ((controlMask          , xK_Print) , mySpawn "sleep 0.2; scrot -s -e 'mv $f ~/common/shots'")
    , ((0                 , xK_Print ), mySpawn "scrot -e 'mv $f ~/common/shots'")
    , ((modm              , xK_f     ), mySpawn $ envVarDefault "BROWSER" "chromium")
    , ((modm              , xK_e     ), mySpawnTerm $ envVarDefault "EDITOR" "vim")
    , ((modm              , xK_t     ), mySpawnTerm "ranger")
    , ((modm .|. shiftMask, xK_t     ), mySpawnTerm "vim ~/documents/todo/todo")
    , ((modm              , xK_a     ), mySpawnTerm "alsamixer")
    , ((modm              , xK_w     ), mySpawnTerm "iw wlan0 scan dump | less")
    , ((modm .|. shiftMask, xK_l     ), mySpawnTerm "journalctl -f")
    , ((modm              , xK_n     ), mySpawnTerm "ncmpcpp")
    , ((modm              , xK_o     ), mySpawnTerm "htop")
    , ((modm .|. shiftMask, xK_m     ), mySpawnTerm "mutt")
        -- for some reason mutt sometimes has trouble rendering if
        -- the shell is not forced to be interactive
        
    -- XF86AudioPrev
    , ((0 , 0x1008ff16)     , spawn "mpc prev")
    -- XF86AudioStop
    , ((0 , 0x1008ff15)     , spawn "mpc stop")
    -- XF86AudioPlay
    , ((0 , 0x1008ff14)     , spawn "mpc toggle")
    -- XF86AudioNext
    , ((0 , 0x1008ff17)     , spawn "mpc next")
    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff11)     , spawn "amixer set Master 5%-")
    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff13)     , spawn "amixer set Master 5%+")
    -- XF86AudioMute
    , ((0 , 0x1008ff12)     , spawn "amixer set Master toggle")
    -- XF86MonBrightnessUp
    , ((0 , 0x1008ff02)     , spawn "xbacklight -inc 5")
    -- XF86MonBrightnessDown
    , ((0 , 0x1008ff03)     , spawn "xbacklight -dec 5")
    -- XF86KbdBrightnessUp TODO 
    , ((0 , 0x1008ff05)     , spawn $ "dbus-send --type=method_call --print-reply=literal " 
      ++ "--system --dest=\"org.freedesktop.UPower\" /org/freedesktop/UPower/KbdBacklight " 
      ++ "org.freedesktop.UPower.KbdBacklight.GetBrightness")
  -- dbus-send --type=method_call --print-reply=literal --system --dest="org.freedesktop.UPower" /org/freedesktop/UPower/KbdBacklight org.freedesktop.UPower.KbdBacklight.SetBrightness int32:2
    --  XF86KbdBrightnessDown
    , ((0 , 0x1008ff06)     , spawn "amixer set Master toggle")

    --   XF86TouchpadToggle
    , ((0 , 0x1008ffa9)     , toggleMouse)
 
    ]

    ++ concat [
    [ ((modm              , k        ), windows $ W.greedyView i),
      ((modm .|. shiftMask, k        ), windows $ W.shift i)
      -- ((modm .|. shiftMask, k        ), windows $ W.greedyView i . W.shift i)
    ] | (i, k) <- zip (XMonad.workspaces conf)
        [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0] ]

    ++
    -- mod-{g,c} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{g,c} %! Move client to screen 1, 2, or 3
    [((modm .|. m,  key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_g, xK_c, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((mod1Mask, button1), (\w ->
        focus w >> float w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod1Mask, button2), (\w ->
        focus w >> windows W.shiftMaster))
    , ((mod1Mask, button3), (\w ->
        focus w >> float w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Layouts:

myLayoutHook = tall ||| TwoPane (3/100) (1/2) ||| noBorders Full ||| Mirror tall
  where tall = Tall 1 (3/100) (1/2) 

------------------------------------------------------------------------
-- Window rules:

myManageHook = (<+>) manageDocks $ composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = dynamicLogWithPP xmobarPP {
--            ppOutput = hPutStrLn xmproc,
--            ppTitle = xmobarColor "#95e454" "" . shorten 50,
--            ppCurrent = xmobarColor "#eadead" "" . wrap "[" "]",
--            -- ppHidden = xmobarColor "#95e454" "",
--            ppOrder = \(ws:_:t:_) -> [ws,t],
--            ppSep = " | "
--         }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with the config we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  -- xmproc <- spawnPipe "/home/austin/.cabal/bin/xmobar /home/austin/.xmonad/xmobar.hs"
  -- xmproc <- spawnPipe "/home/austin/.cabal/bin/xmobar /home/austin/.xmonad/xmobar.hs"
  xmonad =<< dzen defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        handleEventHook = fullscreenEventHook, -- TODO works?


      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
        -- logHook            = myLogHook
    }

-- Utility functions

env = unsafePerformIO getEnvironment
envVarDefault e d = maybe d id $ lookup e env

mySpawn s = spawn $ "nice -n " ++ show myAddNice ++ " " ++ s

mySpawnTerm c = mySpawn (myTerminal ++ " -e " ++ myShell ++ " -ic '" ++ c ++ "'")

-- Handling info bar

-- newtype InfoBar = InfoBar [Handle]

-- Functions for disabling the mouse

newtype MouseEnabled = MouseEnabled Bool
  deriving Typeable

instance ExtensionClass MouseEnabled where
  initialValue = MouseEnabled True

toggleMouse = do
  e <- mouseEnabled
  if e then unsafeDisableMouse else unsafeEnableMouse

enableMouse = do
  e <- mouseEnabled
  if e then return () else unsafeEnableMouse

disableMouse = do
  e <- mouseEnabled
  if e then unsafeDisableMouse else return ()

unsafeEnableMouse :: X ()
unsafeEnableMouse = do
  spawn "synclient TouchpadOff=0"
  XS.put (MouseEnabled True)

unsafeDisableMouse :: X ()
unsafeDisableMouse = do
  spawn "synclient TouchpadOff=1"
  XS.put (MouseEnabled False)

mouseEnabled :: X Bool
mouseEnabled = do
  MouseEnabled b <- XS.get
  return b

