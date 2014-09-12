{-# LANGUAGE DeriveDataTypeable #-}

import System.IO
import System.IO.Unsafe(unsafePerformIO)
import System.Environment(getEnvironment)
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
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
myWorkspaces         = ["1","2","3","4","5","6","7","8","9","0"]
myNormalBorderColor  = "gray" -- "#dddddd"
myFocusedBorderColor = "red" -- "#ff0000"
myAddNice            = 10 -- keep xmonad at high priority

------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), mySpawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), mySpawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), mySpawn "dmenu_term_run")
    , ((modm,               xK_d     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
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

      -- ((controlMask          , xK_Print) , mySpawn "sleep 0.2; scrot -s -e 'mv $f ~/common/shots'")
    , ((0                 , xK_Print) , mySpawn "scrot -e 'mv $f ~/common/shots'")
    , ((modm              , xK_f)     , mySpawn $ envVarDefault "BROWSER" "chromium")
    , ((modm              , xK_e)     , mySpawnTerm $ envVarDefault "EDITOR" "vim")
    , ((modm              , xK_t)     , mySpawnTerm "ranger")
    , ((modm .|. shiftMask, xK_t)     , mySpawnTerm "vim ~/documents/todo/todo")
    , ((modm              , xK_a)     , mySpawnTerm "alsamixer")
    , ((modm              , xK_w)     , mySpawnTerm "iw wlan0 scan dump | less")
    , ((modm .|. shiftMask, xK_l)     , mySpawnTerm "journalctl -f")
    , ((modm              , xK_n)     , mySpawnTerm "ncmpcpp")
    , ((modm              , xK_o)     , mySpawnTerm "htop")
    , ((modm .|. shiftMask, xK_m)     , mySpawnTerm "mutt")
        -- for some reason mutt sometimes has trouble rendering if
        -- the shell is not forced to be interactive
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

myLayoutHook = avoidStruts (tiled ||| Full ||| Mirror tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
myLogHook xmproc = dynamicLogWithPP xmobarPP {
           ppOutput = hPutStrLn xmproc,
           ppTitle = xmobarColor "#95e454" "" . shorten 50,
           ppCurrent = xmobarColor "#eadead" "" . wrap "[" "]",
           -- ppHidden = xmobarColor "#95e454" "",
           ppOrder = \(ws:_:t:_) -> [ws,t],
           ppSep = " | "
        }

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
  xmproc <- spawnPipe "/home/austin/.cabal/bin/xmobar /home/austin/.xmonad/xmobar.hs"
  xmonad $ myConfig xmproc


myConfig xmproc = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook xmproc
    }

-- Utility functions

env = unsafePerformIO getEnvironment
envVarDefault e d = maybe d id $ lookup e env

mySpawn s = spawn $ "nice -n " ++ n ++ " " ++ s
  where n = show myAddNice

mySpawnTerm c = mySpawn (myTerminal ++ " -e " ++ myShell ++ " -ic '" ++ c ++ "'")

-- Functions for disabling the mouse

newtype MouseEnabled = ME Bool
  deriving Typeable

instance ExtensionClass MouseEnabled where
  initialValue = ME True

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
  XS.put (ME True)

unsafeDisableMouse :: X ()
unsafeDisableMouse = do
  spawn "synclient TouchpadOff=1"
  XS.put (ME False)

mouseEnabled :: X Bool
mouseEnabled = do
  ME b <- XS.get
  return b

