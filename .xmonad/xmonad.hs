--
-- xmonad configuration file
--

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.Promote (promote)
import System.Posix.Env (getEnv)
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "gnome-terminal"

-- My preferential browser
--
myBrowser :: String
myBrowser = "firefox"

-- Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth  = 2

-- modMask lets you specify which modkey you want to use. mod4mask is window key
-- I'm used to prefix key because of emacs, stumpwm, conkeror and firefox with keysnail
--
myModMask :: KeyMask
myModMask = mod4Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- Prefix key
--
prefixKey :: String
prefixKey = "C-;"

-- A utility function to compute the final binding with the prefix key
--
prefix :: String -> String
prefix = ((prefixKey ++ " ") ++)

-- Execute a command and show its result in a zenity window dialog
--
spawnZenityCmd :: String -> X ()
spawnZenityCmd = spawn . zenityCmd
                 where zenityCmd :: String -> String
                       zenityCmd cmd = "zenity --info --text \"$(" ++ cmd ++ ")\""

-- Display some data in a zenity window dialog
--
-- spawnZenityText :: String -> X ()
-- spawnZenityText = spawn . zenityText
--                   where zenityText :: String -> String
--                         zenityText s = "zenity --info --text '" ++ s ++ "'"

-- run or raise with a default config folder from which finding the command
--
myRunOrRaise :: String -> String -> Query Bool -> X ()
myRunOrRaise home cmd = runOrRaise (home ++ cmd)

-- My keymap (with prefix keys and all -> need to be transformed through mkKeymap)
--
myKeymap :: String -> XConfig Layout -> [(String, X ())]
myKeymap home conf @(XConfig { terminal   = myTerm
                             , layoutHook = myLayoutHook
                             , workspaces = myWss}) =
  [-- personal script launcher
    (prefix "e",         myRunOrRaise home "/bin/emacs/emacs.sh"                        (className =? "Emacs"))
  --, (prefix "C-x",       myRunOrRaise home "/bin/xephyr/xephyr.sh"                      (className =? "Xephyr")) -- Deactivate xephyr in xmonad (a priori no need)
  , (prefix "y",         myRunOrRaise home "/bin/app/yed.sh"                            (className =? "sun-awt-X11-XFramePeer"))
  , (prefix "S-c",       myRunOrRaise home "/applications/LightTable/LightTable"        (className =? "ltbin"))
  , (prefix "i",         myRunOrRaise home "/bin/ide/idea.sh"                           (className =? "jetbrains-idea-ce"))
  , (prefix "C-x",       myRunOrRaise home "/bin/ide/idea.sh"                           (className =? "jetbrains-idea-ce"))
  , (prefix "S-j",       myRunOrRaise home "/applications/visualvm/bin/visualvm"        (className =? "java-lang-Thread"))
  , (prefix "S-l",       myRunOrRaise home "/applications/sqldeveloper/sqldeveloper.sh" (className =? "sun-awt-X11-XFramePeer"))
    -- swap
  , (prefix prefixKey,   promote)
    -- run or raise commands
  , (prefix "x",         runOrRaise myTerm                     (className =? "Gnome-terminal"))
  , (prefix "S-s",       runOrRaise "cinnamon-settings"        (className =? "Cinnamon-settings.py"))
  , (prefix "S-t",       runOrRaise "totem"                    (className =? "Totem"))
  , (prefix "C-e",       runOrRaise "evince"                   (className =? "Evince"))
  , (prefix "C-i",       runOrRaise "eog"                      (className =? "Eog"))
  , (prefix "d",         runOrRaise "pinta"                    (className =? "Pinta"))
  , (prefix "S-i",       runOrRaise "gimp"                     (className =? "Gimp"))
  , (prefix "C-a",       runOrRaise "audacious"                (className =? "Audacious"))
  , (prefix "M1-j",      runOrRaise "jconsole"                 (className =? "sun-tools-jconsole-JConsole"))
  , (prefix "C-c",       runOrRaise "arduino"                  (className =? "processing-appBase"))
  , (prefix "C-w",       runOrRaise "gksudo wireshark"         (className =? "wireshark"))
  , (prefix "n",         runOrRaise "nemo"                     (className =? "Nemo"))
  , (prefix "S-n",       runOrRaise "thunar"                   (className =? "thunar"))
  , (prefix "C-M1-f",    runOrRaise "filezilla"                (className =? "Filezilla"))
  , (prefix "C-v",       runOrRaise "virtualbox"               (className =? "Qt-subapplication"))
  , (prefix "u",         runOrRaise "unetbootin"               (className =? "unetbootin"))
  , (prefix "/",         runOrRaise "transmission-gtk"         (className =? "transmission-gtk"))
  , (prefix "S-g",       runOrRaise "gksudo /usr/sbin/gparted" (className =? "gpartedbin"))
  , (prefix "S-f",       runOrRaise ""                         (className =? "file_progress"))
  , (prefix "S-x",       runOrRaise "xosview"                  (className =? "xosview"))
  , (prefix "b",         runOrRaise "baobab"                   (className =? "baobab"))
  , (prefix "z",         runOrRaise "gitk"                     (className =? "gitk"))
  , (prefix "S-f",       runOrRaise "fbreader"                 (className =? "fbreader"))
  , (prefix "M1-t",      runOrRaise "tuxguitar"                (className =? "TuxGuitar"))
  , (prefix "C-c",       runOrRaise "skype"                    (className =? "skype"))
  , (prefix "f",         runOrRaise myBrowser                  (className =? "Firefox"))
    -- some commands
  , (prefix "a",         spawnZenityCmd "date")
  , (prefix "S-k",       spawnZenityCmd "ssh-add -l")
  , (prefix "S-e",       spawnZenityCmd "cat /etc/environment")
  , (prefix "S-h",       spawnZenityCmd "cat /etc/hosts")
  , (prefix "C-S-i",     spawnZenityCmd "/sbin/ifconfig")
  , (prefix "S-b",       spawnZenityCmd "acpi -b")
  , (prefix "^",         spawnZenityCmd "top -b -n 1 -c -d 1")
    -- shell command
  , (prefix "C-s",       spawn "scrot -u $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
  , (prefix "M1-s",      spawn "~/bin/touchpad/toggle-touchpad-manual.sh 1; scrot -s $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
  , (prefix "C-t",       spawn "~/bin/touchpad/toggle-touchpad.sh")
  , (prefix "C-S-s",     spawn "gksudo pm-suspend")
  , (prefix "C-S-h",     spawn "gksudo pm-hibernate")
  , (prefix "S-a",       spawn "~/bin/ssh/ssh-add.sh")
    -- , (prefix "p",         spawn "gksudo ~/bin/proxy/proxy.sh on && ~/bin/wifi/nm-applet.sh stop")
    -- , (prefix "S-p",       spawn "gksudo ~/bin/proxy/proxy.sh off && ~/bin/wifi/nm-applet.sh stop")
  , (prefix "C-b",       spawn "~/bin/brightness/dec-brightness.sh 5")
  , (prefix "C-f",       spawn "~/bin/brightness/inc-brightness.sh 5")
  , (prefix "C-S-m",     spawn "~/bin/brightness/half-brightness.sh")
  , (prefix "S-m",       spawn "~/bin/brightness/max-brightness.sh")
  , (prefix "M1-f",      spawn "exec amixer set Master 5%+")
  , (prefix "M1-b",      spawn "exec amixer set Master 5%-")
  , (prefix "M1-m",      spawn "exec amixer set Master toggle")
  , (prefix "C-o",       spawn "~/bin/wifi/wifi-off.sh")
  , (prefix "S-o",       spawn "~/bin/wifi/wifi-on.sh")
    -- , (prefix "C-p",       spawn "~/bin/service/service.sh restart stalonetray -t --window-type=normal")
  , (prefix "C-M1-l",    spawn "~/bin/session/lock.sh")
  , (prefix "\\",        spawn "evince ~/books/haskell/algorithms-a-functional-programming-haskell-approach.pdf")
    -- dmenu
  , (prefix "s",         search)
  , (prefix "r",         spawn $ dmenuCmd myXPConfig)
  , (prefix "S-1",       spawn "gmrun")                           -- another menu launcher (equivalent to F2 in gnome2)
  , (prefix "g",         windowPromptGoto myXPConfig)             -- prompt to help in selecting window to move to
  , (prefix "M1-x",      xmonadPrompt myXPConfig)                 -- a prompt to show the current possible commands
  , (prefix "c", kill)                                            -- close focused window
  , (prefix "<Space>",   sendMessage NextLayout)                  -- Rotate through the available layout algorithms
  , (prefix "C-<Space>", setLayout myLayoutHook)                  -- Reset the layouts on the current workspace to default
  , (prefix "M1-n",      refresh)                                 -- Resize viewed windows to the correct size
  , (prefix "<Tab>",     windows W.focusDown)                     -- Move focus to the next window
  , (prefix "j",         windows W.focusDown)                     -- Move focus to the next window
  , (prefix "k",         windows W.focusUp)                       -- Move focus to the previous window
  , (prefix "m",         windows W.focusMaster)                   -- Move focus to the master window
  , (prefix "<Return>",  windows W.swapMaster)                    -- Swap the focused window and the master window
  , (prefix "C-j",       windows W.swapDown)                      -- Swap the focused window with the next window
  , (prefix "C-k",       windows W.swapUp)                        -- Swap the focused window with the previous window
  , (prefix "M1-l",      sendMessage Shrink)                      -- Shrink the master area
  , (prefix "M1-h",      sendMessage Expand)                      -- Expand the master area
  , (prefix "t",         withFocused $ windows . W.sink)          -- Push window back into tiling
  , (prefix "h",         sendMessage (IncMasterN 1))              -- Increment the number of windows in the master area
  , (prefix "l",         sendMessage (IncMasterN (-1)))           -- Deincrement the number of windows in the master area
  , (prefix "S-q",       recompile True >> restart "xmonad" True) -- reload the setup from xmonad
  , (prefix "M1-q",      io exitSuccess)] ++                      -- Quit xmonad
  -- M1-n - Switch to workspace with id n
  -- S-n  - Move the client to workspace with id n
  [(prefix $ pk ++ k, windows $ f i) | (i, k) <- zip myWss $ map show ([1..9] :: [Integer])
                                     , (f, pk) <- [(W.greedyView, "M1-"), (W.shift, "S-")]]
  where searchSite = S.promptSearchBrowser myXPConfig myBrowser
        search     = SM.submap . mkKeymap conf $
                     [("g", searchSite S.google)
                     ,("h", searchSite S.hoogle)
                     ,("a", searchSite S.amazon)
                     ,("i", searchSite S.imdb)
                     ,("y", searchSite S.youtube)
                     ,("w", searchSite S.wikipedia)]

-- Key bindings
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf = mkKeymap conf (myKeymap home conf)

------------------------------------------------------------------------
-- mouse bindings: default actions bound to mouse events
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
  where -- default tiling algorithm partitions the screen into two panes
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
               , workspaceIde] ++
               map show ([7..9] :: [Integer])

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
    , className =? "Zenity"           --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , className =? "Emacs"            --> doShift workspaceEmacs
    , className =? "Gnome-terminal"   --> doShift workspaceTerminal
    , className =? "jetbrains-ide-ce" --> doShift workspaceIde
    , className =? "Firefox"          --> doShift workspaceWeb
    ]

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
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()

-- My font to try and use everywhere
--
myDefaultFont :: String
myDefaultFont = "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"
myFocusedBorderColor :: String
myFocusedBorderColor = "#5193f7"
myNormalTextColor :: String
myNormalTextColor = "#ffffff"

-- Default configuration for prompt
--
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
              { font        = myDefaultFont
              , bgColor     = myColor0
              , fgColor     = myColor1
              , bgHLight    = myColor1
              , fgHLight    = myColor0
              , borderColor = myNormalBorderColor
              , position    = Top
              , historySize = 256
              , promptBorderWidth = 1}
              where myColor0 = myFocusedBorderColor
                    myColor1 = myNormalTextColor

-- dmenu_run command with some specific format (my font for one)
--
dmenuCmd :: XPConfig -> String
dmenuCmd (XPC { font        = myFont
              , bgColor     = myBgColor
              , fgColor     = myFgColor
              , bgHLight    = myBgHlight
              , fgHLight    = myFgHLight}) =
  "dmenu_run -i -fn '" ++ myFont ++ "' -nb '" ++ myBgColor ++ "' -nf '" ++ myFgColor ++ "' -sb '" ++ myBgHlight ++ "' -sf '"++ myFgHLight ++ "' -p 'Run:'"

-- Spawn multiple services (restart them if already started)
--
spawnCommands :: [String] -> IO ()
spawnCommands = mapM_ $ spawn . (\ service -> "~/bin/service/service.sh restart " ++ service ++ " &")

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main :: IO ()
main = do
  Just home <- getEnv "HOME"
  xmproc <- spawnPipe "xmobar"
  spawnCommands [ "nemo --no-default-window"
                , "xscreensaver"
                , "dropbox start"
                , "nm-applet"
                , "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000"]
  xmonad $ desktopConfig {
                -- Simple stuff
                  terminal           = myTerminal
                , focusFollowsMouse  = myFocusFollowsMouse
                , borderWidth        = myBorderWidth
                , modMask            = myModMask
                , workspaces         = myWorkspaces
                , normalBorderColor  = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor

                -- key bindings
                , keys               = myKeys home
                , mouseBindings      = myMouseBindings

              -- hooks, layouts
                , layoutHook         = avoidStruts myLayout
                , manageHook         = manageDocks <+> myManageHook
                , handleEventHook    = myEventHook
                -- Status bars and logging
                -- Perform an arbitrary action on each internal state change or X event.
                -- See the 'XMonad.Hooks.DynamicLog' extension for examples.
                --
                , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50}
                , startupHook        = myStartupHook
            }
