{-# LANGUAGE DeriveDataTypeable #-}

import System.IO
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

myTerminal      = "xterm"
myBorderWidth   = 1
myModMask       = mod4Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_term_run")
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
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_period), sendMessage (IncMasterN 1))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- , ((modm              , xK_apostrophe), toggleMouse)
    , ((modm              , xK_semicolon), toggleMouse)

      -- ((controlMask          , xK_Print) , spawn "sleep 0.2; scrot -s -e 'mv $f ~/common/shots'")
    , ((0                 , xK_Print) , spawn "scrot -e 'mv $f ~/common/shots'")
    , ((modm              , xK_f)     , spawn "luakit")
    , ((modm              , xK_r)     , spawn $ myTerminal ++ " -e ranger")
    , ((modm              , xK_a)     , spawn $ myTerminal ++ " -e alsamixer")
    -- , ((modm              , xK_w)     , spawn $ myTerminal ++ " -e wicd-curses")
    , ((modm              , xK_w)     , spawn $ myTerminal ++ " -e zsh -ic 'iw wlan0 scan dump | less'")
    , ((modm .|. shiftMask, xK_l)     , spawn $ myTerminal ++ " -e zsh -ic 'journalctl -f'")
    , ((modm              , xK_n)     , spawn $ myTerminal ++ " -e ncmpcpp")
    , ((modm              , xK_o)     , spawn $ myTerminal ++ " -e htop")
    , ((modm .|. shiftMask, xK_m)     , spawn $ myTerminal ++ " -e zsh -ic mutt")
        -- for some reason mutt sometimes has trouble rendering if
        -- the shell is not forced to be interactive
    ]

    ++ concat [
    [ ((modm              , k        ), windows $ W.greedyView i),
      ((modm .|. shiftMask, k        ), windows $ W.shift i)
      -- ((modm .|. shiftMask, k        ), windows $ W.greedyView i . W.shift i)
    ] | (i, k) <- zip (XMonad.workspaces conf)
        [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0] ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((mod1Mask, button1), (\w ->
        focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod1Mask, button2), (\w ->
        focus w >> windows W.shiftMaster))
    , ((mod1Mask, button3), (\w ->
        focus w >> mouseResizeWindow w >> windows W.shiftMaster))
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
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  -- xmproc <- spawnPipe "/home/austin/.cabal/bin/xmobar /home/austin/.xmonad/xmobar.hs"
  xmproc <- spawnPipe "/home/austin/.cabal/bin/xmobar /home/austin/.xmonad/xmobar.hs"
  xmonad $ defaults xmproc

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults xmproc = defaultConfig {
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
  spawn "synclient TouchpadOff=0 && killall unclutter"
  XS.put (ME True)

unsafeDisableMouse :: X ()
unsafeDisableMouse = do
  spawn "synclient TouchpadOff=1 && unclutter -grab -idle 600 -jitter 100000"
  XS.put (ME False)

mouseEnabled :: X Bool
mouseEnabled = do
  ME b <- XS.get
  return b

