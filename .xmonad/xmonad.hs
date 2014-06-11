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
import XMonad.Actions.WindowGo (runOrRaiseNext)
import XMonad.Actions.Promote (promote)
import System.Posix.Env (getEnv)
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad (xmonadPromptC)
import XMonad.Prompt.AppLauncher (launchApp)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)

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

-- | Whether a mouse click select the focus or is just passed to the window
--
myClickJustFocuses :: Bool
myClickJustFocuses = True

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
myRunOrRaise home cmd = runOrRaiseNext (home ++ cmd)

-- My keymap as (prefix keybindings, command description, command)
--
myKeymapWithDescription :: String -> XConfig Layout -> [(String, String, X ())]
myKeymapWithDescription home conf @(XConfig { terminal   = myTerm
                             , layoutHook = myLayoutHook
                             , workspaces = myWss}) =
  [ (prefix "C-g"       , "abort"                      , spawn "xdotool key Escape")
  , (prefix "e"         , "emacs"                      , myRunOrRaise home "/bin/emacs/emacs.sh"                        (className =? "Emacs"))
  , (prefix "C-x"       , "xephyr"                     , myRunOrRaise home "/bin/xephyr/xephyr-stumpwm.sh"              (className =? "Xephyr"))
  , (prefix "y"         , "yed"                        , myRunOrRaise home "/bin/app/yed.sh"                            (className =? "sun-awt-X11-XFramePeer"))
  , (prefix "S-c"       , "lighttable"                 , myRunOrRaise home "/applications/LightTable/LightTable"        (className =? "ltbin"))
  , (prefix "i"         , "ide"                        , myRunOrRaise home "/bin/ide/idea.sh"                           (className =? "jetbrains-idea-ce"))
  , (prefix "S-j"       , "visualvm"                   , myRunOrRaise home "/applications/visualvm/bin/visualvm"        (className =? "java-lang-Thread"))
  , (prefix "S-l"       , "sqldeveloper"               , myRunOrRaise home "/applications/sqldeveloper/sqldeveloper.sh" (appName =? "sun-awt-X11-XFramePeer"))
  , (prefix prefixKey   , "promote"                    , promote)
  , (prefix "x"         , "terminal"                   , runOrRaiseNext myTerm                     (className =? "Gnome-terminal"))
  , (prefix "S-s"       , "desktop-settings"           , runOrRaiseNext "cinnamon-settings"        (className =? "Cinnamon-settings.py"))
  , (prefix "S-t"       , "video-player"               , runOrRaiseNext "totem"                    (className =? "Totem"))
  , (prefix "C-e"       , "pdf-reader"                 , runOrRaiseNext "evince"                   (className =? "Evince"))
  , (prefix "C-i"       , "image-viewer"               , runOrRaiseNext "eog"                      (className =? "Eog"))
  , (prefix "d"         , "pinta"                      , runOrRaiseNext "pinta"                    (className =? "Pinta"))
  , (prefix "S-i"       , "gimp"                       , runOrRaiseNext "gimp"                     (className =? "Gimp"))
  , (prefix "C-a"       , "music-reader"               , runOrRaiseNext "audacious"                (className =? "Audacious"))
  , (prefix "M1-j"      , "jconcole"                   , runOrRaiseNext "jconsole"                 (className =? "sun-tools-jconsole-JConsole"))
  , (prefix "C-c"       , "arduino-ide"                , runOrRaiseNext "arduino"                  (className =? "processing-appBase"))
  , (prefix "C-w"       , "wireshark"                  , runOrRaiseNext "gksudo wireshark"         (className =? "wireshark"))
  , (prefix "n"         , "nemo"                       , runOrRaiseNext "nemo"                     (className =? "Nemo"))
  , (prefix "S-n"       , "thunar"                     , runOrRaiseNext "thunar"                   (className =? "thunar"))
  , (prefix "C-M1-f"    , "filezilla"                  , runOrRaiseNext "filezilla"                (className =? "Filezilla"))
  , (prefix "C-v"       , "virtualbox"                 , runOrRaiseNext "virtualbox"               (className =? "Qt-subapplication"))
  , (prefix "u"         , "unetbootin"                 , runOrRaiseNext "unetbootin"               (className =? "unetbootin"))
  , (prefix "/"         , "transmission"               , runOrRaiseNext "transmission-gtk"         (className =? "transmission-gtk"))
  , (prefix "S-g"       , "gparted"                    , runOrRaiseNext "gksudo /usr/sbin/gparted" (className =? "gpartedbin"))
  , (prefix "S-f"       , "file-progression"           , runOrRaiseNext ""                         (className =? "file_progress"))
  , (prefix "S-x"       , "xosview"                    , runOrRaiseNext "xosview"                  (className =? "xosview"))
  , (prefix "b"         , "baobab"                     , runOrRaiseNext "baobab"                   (className =? "baobab"))
  , (prefix "z"         , "gitk"                       , runOrRaiseNext "gitk"                     (className =? "gitk"))
  , (prefix "S-f"       , "fbreader"                   , runOrRaiseNext "fbreader"                 (className =? "fbreader"))
  , (prefix "M1-t"      , "tuxguitar"                  , runOrRaiseNext "tuxguitar"                (className =? "TuxGuitar"))
  , (prefix "C-c"       , "skype"                      , runOrRaiseNext "skype"                    (className =? "skype"))
  , (prefix "f"         , "browser"                    , runOrRaiseNext myBrowser                  (className =? "Firefox"))
  , (prefix "a"         , "date"                       , spawnZenityCmd "date")
  , (prefix "S-k"       , "ssh-add-l"                  , spawnZenityCmd "ssh-add -l")
  , (prefix "S-e"       , "cat-etc-environment"        , spawnZenityCmd "cat /etc/environment")
  , (prefix "S-h"       , "cat-etc-hosts"              , spawnZenityCmd "cat /etc/hosts")
  , (prefix "C-S-i"     , "sbin-ifconfig"              , spawnZenityCmd "/sbin/ifconfig")
  , (prefix "S-b"       , "acpi"                       , spawnZenityCmd "acpi -b")
  , (prefix "^"         , "top"                        , spawnZenityCmd "top -b -n 1 -c -d 1")
  , (prefix "C-s"       , "print-screen"               , spawn "scrot -u $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
  , (prefix "M1-s"      , "mouse-print-screen"         , spawn "~/bin/touchpad/toggle-touchpad-manual.sh 1; scrot -s $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png")
  , (prefix "C-t"       , "toggle-touchpad"            , spawn "~/bin/touchpad/toggle-touchpad.sh")
  , (prefix "C-S-s"     , "pm-suspend"                 , spawn "gksudo pm-suspend")
  , (prefix "C-S-h"     , "pm-hibernate"               , spawn "gksudo pm-hibernate")
  , (prefix "S-a"       , "ssh-add"                    , spawn "~/bin/ssh/ssh-add.sh")
  , (prefix "C-b"       , "brightness-decrease"        , spawn "~/bin/brightness/dec-brightness.sh 5")
  , (prefix "C-f"       , "brightness-increase"        , spawn "~/bin/brightness/inc-brightness.sh 5")
  , (prefix "C-S-m"     , "brightness-half"            , spawn "~/bin/brightness/half-brightness.sh")
  , (prefix "S-m"       , "brightness-max"             , spawn "~/bin/brightness/max-brightness.sh")
  , (prefix "M1-f"      , "sound-increase"             , spawn "exec amixer set Master 5%+")
  , (prefix "M1-b"      , "sound-decrease"             , spawn "exec amixer set Master 5%-")
  , (prefix "M1-m"      , "sound-toggle"               , spawn "exec amixer set Master toggle")
  , (prefix "C-o"       , "wifi-off"                   , spawn "~/bin/wifi/wifi-off.sh")
  , (prefix "S-o"       , "wifi-on"                    , spawn "~/bin/wifi/wifi-on.sh")
  , (prefix "C-M1-l"    , "session-lock"               , spawn "~/bin/session/lock.sh")
  , (prefix "M1-e"      , "evince-prompt"              , launchApp myXPConfig "evince")
  , (prefix "s"         , "search-url"                 , search)
  , (prefix "r"         , "dmenu"                      , spawn $ dmenuCmd myXPConfig)
  , (prefix "M1-r"      , "exec"                       , runOrRaisePrompt myXPConfig)
  , (prefix "g"         , "goto"                       , windowPromptGoto myXPConfig)
  , (prefix "M1-x"      , "meta-x"                     , xmonadPromptC keymapDescription myXPConfig)
  , (prefix "c"         , "close-current-window"       , kill)
  , (prefix "<Space>"   , "rotate-layout"              , sendMessage NextLayout)
  , (prefix "C-<Space>" , "reset-layout"               , setLayout myLayoutHook)
  , (prefix "M1-n"      , "refresh"                    , refresh)
  , (prefix "<Tab>"     , "window-move-focus-next"     , windows W.focusDown)
  , (prefix "j"         , "window-move-focus-next"     , windows W.focusDown)
  , (prefix "k"         , "window-move-focus-previous" , windows W.focusUp)
  , (prefix "m"         , "window-move-focus-master"   , windows W.focusMaster)
  , (prefix "<Return>"  , "window-swap-focus-master"   , windows W.swapMaster)
  , (prefix "C-j"       , "window-swap-focus-next"     , windows W.swapDown)
  , (prefix "C-k"       , "window-swap-focus-previous" , windows W.swapUp)
  , (prefix "M1-l"      , "master-shrink-area"         , sendMessage Shrink)
  , (prefix "M1-h"      , "master-expand-area"         , sendMessage Expand)
  , (prefix "t"         , "window-push-back-tiling"    , withFocused $ windows . W.sink)
  , (prefix "h"         , "window-inc-num-master-area" , sendMessage (IncMasterN 1))
  , (prefix "l"         , "window-dec-num-master-area" , sendMessage (IncMasterN (-1)))
  , (prefix "S-q"       , "xmonad-restart"             , broadcastMessage ReleaseResources >> recompile True >> restart "xmonad" True)
  , (prefix "M1-q"      , "xmonad-quit"                , io exitSuccess)] ++
  -- M1-n - Switch to workspace with id n
  -- S-n  - Move the client to workspace with id n
  [(prefix $ pk ++ k, desc ++ k , windows $ f i) | (i, k) <- zip myWss $ map show [1 .. length myWss]
                                                 , (f, pk, desc) <- [ (W.greedyView, "M1-", "workspace-switch-to-id-")
                                                                    , (W.shift, "S-", "workspace-move-client-to-id-")]]
  where -- Permits the search through the system browser
        searchSite = S.promptSearchBrowser myXPConfig myBrowser
        search     = SM.submap . mkKeymap conf $
                     [ ("g", searchSite S.google)
                     , ("h", searchSite S.hoogle)
                     , ("a", searchSite S.amazon)
                     , ("i", searchSite S.imdb)
                     , ("y", searchSite S.youtube)
                     , ("w", searchSite S.wikipedia)]
        -- Rework the keymap description to extract the command description and the associated actions
        keymapDescription = map (\ (_, xmonadActionDesc, xmonadAction) -> (xmonadActionDesc, xmonadAction)) fullKeymap
        fullKeymap = myKeymapWithDescription home conf

-- Key bindings
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf = mkKeymap conf keymap
                   where keymap = map (\ (keybinding, _, xmonadAction) -> (keybinding, xmonadAction)) fullKeymap
                         fullKeymap = myKeymapWithDescription home conf
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
                                      >> windows W.shiftMaster)]
    -- you may also bind events to the mouse scroll wheel (button4 and button5)

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

workspaceFloat :: String
workspaceFloat = "7:ide"

workspaceBooks :: String
workspaceBooks = "8:books"

myWorkspaces :: [String]
myWorkspaces = [ workspaceEmacs
               , workspaceTerminal
               , workspaceWeb
               , workspaceCode
               , workspaceIrc
               , workspaceIde
               , workspaceFloat
               , workspaceBooks]

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
-- 'className' and 'appName' (`resource`) are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"          --> doShift workspaceFloat >> doFloat
    , className =? "Gimp"             --> doShift workspaceFloat >> doFloat
    , className =? "Zenity"           --> doFloat
    , appName  =? "desktop_window"    --> doIgnore
    , appName  =? "kdesktop"          --> doIgnore
    , className =? "Emacs"            --> doShift workspaceEmacs
    , className =? "Gnome-terminal"   --> doShift workspaceTerminal
    , className =? "jetbrains-ide-ce" --> doShift workspaceIde
    , className =? "Firefox"          --> doShift workspaceWeb
    , className =? "Evince"           --> doShift workspaceBooks]

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
myXPConfig                        = defaultXPConfig
              { font              = myDefaultFont
              , bgColor           = myColor0
              , fgColor           = myColor1
              , bgHLight          = myColor1
              , fgHLight          = myColor0
              , borderColor       = myNormalBorderColor
              , position          = Top
              , historySize       = 256
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
                , clickJustFocuses   = myClickJustFocuses
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
                , logHook            = dynamicLogWithPP $ xmobarPP
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor "green" "" . shorten 50}
                , startupHook        = myStartupHook
            }
