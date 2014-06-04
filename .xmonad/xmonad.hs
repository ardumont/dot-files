--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "gnome-terminal"

-- Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth  = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
-- I'm used to prefix key because of emacs, stumpwm, conkeror and firefox with keysnail
-- So, until I found out how to use xmonad the same way (with C-; as a prefix key)
-- I prefer using windows key
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = map show [1..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"
myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- Prefix key
--
prefix :: String -> String
prefix key = "C-; " ++ key

-- keybinding
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = \conf -> mkKeymap conf $
     [
    -- terminal
      (prefix "x", spawn $ terminal conf)
    -- some help message
    , (prefix "h", spawn "xmessage 'hello world!!! using prefix key in haskell!'")
    -- reload the setup from xmonad
    , (prefix "L", spawn "xmonad --recompile; xmonad --restart")
    -- spawning firefox
    , (prefix "f", spawn "firefox")
    -- dmenu
    , (prefix "p", spawn "dmenu_run")
    -- another menu
    , (prefix "C-p", spawn "gmrun")
    -- emacs client (emacs daemon is launched at startup)
    , (prefix "e", spawn "emacsclient -c")
    -- close focused window
    , (prefix "c", kill)
    -- Rotate through the available layout algorithms
    , (prefix "<Space>", sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , (prefix "C-<Space>", setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , (prefix "n", refresh)
    -- Move focus to the next window
    , (prefix "<Tab>", windows W.focusDown)
    -- Move focus to the next window
    , (prefix "j", windows W.focusDown)
    -- Move focus to the previous window
    , (prefix "k", windows W.focusUp  )
    -- Move focus to the master window
    , (prefix "m", windows W.focusMaster  )
    -- Swap the focused window and the master window
    , (prefix "<Return>", windows W.swapMaster)
    -- Swap the focused window with the next window
    , (prefix "C-j", windows W.swapDown  )
    -- Swap the focused window with the previous window
    , (prefix "C-k", windows W.swapUp    )
    -- Shrink the master area
    , (prefix "C-h", sendMessage Shrink)
    -- Expand the master area
    , (prefix "C-l", sendMessage Expand)
    -- Push window back into tiling
    , (prefix "t", withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , (prefix ",", sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , (prefix ":", sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , (prefix "C-Q", io (exitWith ExitSuccess))
    ]
    -- ++

    -- --
    -- -- mod-[1..9], Switch to workspace N
    -- -- mod-shift-[1..9], Move client to workspace N
    -- --
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    -- --
    -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
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

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults = defaultConfig {
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
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

 -- <Backspace>
 -- <Tab>
 -- <Return>
 -- <Pause>
 -- <Scroll_lock>
 -- <Sys_Req>
 -- <Print>
 -- <Escape>, <Esc>
 -- <Delete>
 -- <Home>
 -- <Left>, <L>
 -- <Up>, <U>
 -- <Right>, <R>
 -- <Down>, <D>
 -- <Page_Up>
 -- <Page_Down>
 -- <End>
 -- <Insert>
 -- <Break>
 -- <Space>
 -- <F1>-<F24>
 -- <KP_Space>
 -- <KP_Tab>
 -- <KP_Enter>
 -- <KP_F1>
 -- <KP_F2>
 -- <KP_F3>
 -- <KP_F4>
 -- <KP_Home>
 -- <KP_Left>
 -- <KP_Up>
 -- <KP_Right>
 -- <KP_Down>
 -- <KP_Prior>
 -- <KP_Page_Up>
 -- <KP_Next>
 -- <KP_Page_Down>
 -- <KP_End>
 -- <KP_Begin>
 -- <KP_Insert>
 -- <KP_Delete>
 -- <KP_Equal>
 -- <KP_Multiply>
 -- <KP_Add>
 -- <KP_Separator>
 -- <KP_Subtract>
 -- <KP_Decimal>
 -- <KP_Divide>
 -- <KP_0>-<KP_9>
