--
-- xmonad configuration file
--

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig
import XMonad.Actions.WindowGo (runOrRaise)
import System.Posix.Env (getEnv)

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
prefix key = "C-t " ++ key

zenityText :: String -> String
zenityText s = "zenity --info --text '" ++ s ++ "'"

zenityCmd :: String -> String
zenityCmd cmd = "zenity --info --text \"$(" ++ cmd ++ ")\""

myRunOrRaise :: String -> String -> Query Bool -> X ()
myRunOrRaise home cmd = runOrRaise (home ++ cmd)

-- keybinding
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf =
  mkKeymap conf [-- personal script launcher
                   (prefix "e",   myRunOrRaise home "/bin/emacs/emacs.sh"                 (className =? "Emacs"))
                 , (prefix "C-x", myRunOrRaise home "/bin/xephyr/xephyr.sh"               (className =? "Xephyr"))
                 , (prefix "y",   myRunOrRaise home "/bin/app/yed.sh"                     (className =? "sun-awt-X11-XFramePeer"))
                 , (prefix "S-c", myRunOrRaise home "/applications/LightTable/LightTable" (className =? "ltbin"))
                 , (prefix "i",   myRunOrRaise home "/bin/ide/idea.sh"                    (className =? "jetbrains-idea-ce"))
                 , (prefix "j",   myRunOrRaise home "/applications/visualvm/bin/visualvm" (className =? "java-lang-Thread"))
                 -- run or raise commands
                 , (prefix "x",   runOrRaise (terminal conf)                              (className =? "Gnome-terminal"))
                 , (prefix ",",   runOrRaise "/usr/bin/cinnamon-settings"                 (className =? "Cinnamon-settings.py"))
                 , (prefix ".",   runOrRaise "/usr/bin/totem"                             (className =? "Totem"))
                 , (prefix "C-e", runOrRaise "/usr/bin/evince"                            (className =? "Evince"))
                 , (prefix "C-i", runOrRaise "/usr/bin/eog"                               (className =? "Eog"))
                 , (prefix "d",   runOrRaise "/usr/bin/pinta"                             (className =? "Pinta"))
                 , (prefix "S-i", runOrRaise "/usr/bin/gimp"                              (className =? "Gimp"))
                 , (prefix "C-a", runOrRaise "/usr/bin/audacious"                         (className =? "Audacious"))
                 , (prefix "C-j", runOrRaise "/usr/bin/jconsole"                          (className =? "sun-tools-jconsole-JConsole"))
                 , (prefix "C-c", runOrRaise "/usr/bin/arduino"                           (className =? "processing-appBase"))
                 , (prefix "C-w", runOrRaise "gksudo /usr/bin/wireshark"                  (className =? "wireshark"))
                 , (prefix "n",   runOrRaise "/usr/bin/nemo"                              (className =? "nemo"))
                 , (prefix "S-n", runOrRaise "/usr/bin/thunar"                            (className =? "thunar"))
                 , (prefix "C-f", runOrRaise "/usr/bin/filezilla"                         (className =? "filezilla"))
                 , (prefix "C-v", runOrRaise "/usr/bin/virtualbox"                        (className =? "Qt-subapplication"))
                 , (prefix "u",   runOrRaise "/usr/bin/unetbootin"                        (className =? "unetbootin"))
                 , (prefix "/",   runOrRaise "/usr/bin/transmission-gtk"                  (className =? "transmission-gtk"))
                 , (prefix "S-g", runOrRaise "gksudo /usr/sbin/gparted"                   (className =? "gpartedbin"))
                 , (prefix "S-f", runOrRaise ""                                           (className =? "file_progress"))
                 , (prefix "S-x", runOrRaise "/usr/bin/xosview"                           (className =? "xosview"))
                 , (prefix "b",   runOrRaise "/usr/bin/baobab"                            (className =? "baobab"))
                 , (prefix "z",   runOrRaise "/usr/bin/gitk"                              (className =? "gitk"))
                 , (prefix "C-f", runOrRaise "/usr/bin/fbreader"                          (className =? "fbreader"))
                 , (prefix "M-r", runOrRaise "/usr/bin/tuxguitar"                         (className =? "TuxGuitar"))
                 , (prefix "C-c", runOrRaise "/usr/bin/skype"                             (className =? "skype"))
                 -- spawning firefox
                 , (prefix "f",   runOrRaise "/usr/bin/firefox"                           (className =? "Firefox"))
                   -- some show message
                 , (prefix "h",   spawn . zenityText $ "hello")
                   -- some commands
                 , (prefix "S-k",   spawn . zenityCmd $ "ssh-add -l")
                 , (prefix "S-e",   spawn . zenityCmd $ "cat /etc/environment")
                 , (prefix "S-h",   spawn . zenityCmd $ "cat /etc/hosts")
                 , (prefix "S-i",   spawn . zenityCmd $ "/sbin/ifconfig")
                 , (prefix "S-b",   spawn . zenityCmd $ "/usr/bin/acpi -b")
                 , (prefix "^",     spawn . zenityCmd $ "top -b -n 1 -c -d 1")
                 , (prefix "C-s",   spawn . zenityCmd $ "/usr/bin/scrot -u $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
                 , (prefix "M-s",   spawn . zenityCmd $ "~/bin/touchpad/toggle-touchpad-manual.sh 1; /usr/bin/scrot -s $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
                 , (prefix "C-S-s", spawn . zenityCmd $ "gksudo pm-suspend")
                 , (prefix "C-S-h", spawn . zenityCmd $ "gksudo pm-hibernate")
                 , (prefix "S-a",   spawn . zenityCmd $ "~/bin/ssh/ssh-add.sh")
                 , (prefix "p",     spawn . zenityCmd $ "gksudo ~/bin/proxy/proxy.sh on && ~/bin/wifi/nm-applet.sh stop")
                 , (prefix "S-p",   spawn . zenityCmd $ "gksudo ~/bin/proxy/proxy.sh off && ~/bin/wifi/nm-applet.sh stop")
                 , (prefix "C-b",   spawn . zenityCmd $ "~/bin/brightness/dec-brightness.sh 5")
                 , (prefix "C-f",   spawn . zenityCmd $ "~/bin/brightness/inc-brightness.sh 5")
                 , (prefix "m",     spawn . zenityCmd $ "~/bin/brightness/min-brightness.sh")
                 , (prefix "C-S-m", spawn . zenityCmd $ "~/bin/brightness/half-brightness.sh")
                 , (prefix "S-m",   spawn . zenityCmd $ "~/bin/brightness/max-brightness.sh")
                 , (prefix "M-f",   spawn . zenityCmd $ "exec amixer set Master 5%+")
                 , (prefix "M-b",   spawn . zenityCmd $ "exec amixer set Master 5%-")
                 , (prefix "M-m",   spawn . zenityCmd $ "exec amixer set Master toggle")
                 , (prefix "C-o",   spawn . zenityCmd $ "~/bin/wifi/wifi-off.sh")
                 , (prefix "S-o",   spawn . zenityCmd $ "~/bin/wifi/wifi-on.sh")
                 , (prefix "C-p",   spawn . zenityCmd $ "~/bin/service/service.sh restart stalonetray -t --window-type=normal")
                 , (prefix "C-M-l", spawn . zenityCmd $ "~/bin/session/lock.sh")
                 , (prefix "'",     spawn . zenityCmd $ "evince ~/books/haskell/algorithms-a-functional-programming-haskell-approach.pdf")
                 -- reload the setup from xmonad
                 , (prefix "S-l", recompile True >> restart "/usr/bin/xmonad" True)
                 -- dmenu
                 , (prefix "p",     spawn "dmenu_run")
                 -- another menu launcher (equivalent to F2 in gnome2)
                 , (prefix "C-p",   spawn "gmrun")
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
                 , (prefix "C-i", windows W.focusDown)
                 -- Move focus to the next window
                 , (prefix "j", windows W.focusDown)
                 -- Move focus to the previous window
                 , (prefix "k", windows W.focusUp)
                 -- Move focus to the master window
                 , (prefix "m", windows W.focusMaster)
                 -- Swap the focused window and the master window
                 , (prefix "<Return>", windows W.swapMaster)
                 -- Swap the focused window and the master window
                 , (prefix "C-m", windows W.swapMaster)
                 -- Swap the focused window with the next window
                 , (prefix "C-j", windows W.swapDown)
                 -- Swap the focused window with the previous window
                 , (prefix "C-k", windows W.swapUp)
                 -- Shrink the master area
                 , (prefix "C--", sendMessage Shrink)
                 -- Expand the master area
                 , (prefix "C-+", sendMessage Expand)
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
                 , (prefix "C-S-q", io exitSuccess)]
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
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
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

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

workspaceEmacs :: String
workspaceEmacs = "1:emacs"

workspaceTerminal :: String
workspaceTerminal = "2:terminal"

workspaceWeb :: String
workspaceWeb = "3:web"

workspaceCode :: String
workspaceCode = "4:code"

workspaceIrc :: String
workspaceIrc = "5:irc"

workspaceIde :: String
workspaceIde = "6:ide"

myWorkspaces :: [String]
myWorkspaces = [ workspaceEmacs
               , workspaceTerminal
               , workspaceWeb
               , workspaceCode
               , workspaceIrc
               , workspaceIde] ++ map show [7..9]

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
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , className =? "Emacs"            --> doShift workspaceEmacs
    , className =? "Gnome-terminal"   --> doShift workspaceTerminal
    , className =? "jetbrains-ide-ce" --> doShift workspaceIde
    , className =? "Firefox"          --> doShift workspaceWeb]

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

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.

-- myFont :: String
-- myFont = "xft:DejaVu Sans:size=10"
-- --myFont = "xft:Ubuntu:weight=bold:size=10"

-- myBgColor :: String
-- myBgColor = "#001070"

-- myFgColor :: String
-- myFgColor = "#bbbbdd"

-- myBgHLight :: String
-- myBgHLight = "#4444aa"

-- myFgHLight :: String
-- myFgHLight = "#ddddff"

-- myXPConfig :: XPConfig
-- myXPConfig = defaultXPConfig
--               { font        = myFont
--               , bgColor     = myBgColor
--               , fgColor     = myFgColor
--               , bgHLight    = myBgHLight
--               , fgHLight    = myFgHLight
--               , borderColor = myNormalBorderColor}

--
defaults :: String -> XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults home = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys home,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main :: IO ()
main =
    getEnv "HOME" >>= \ (Just home) -> xmonad $ defaults home
