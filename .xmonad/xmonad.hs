import qualified Data.Map as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           Control.Monad (liftM)
import           XMonad
import           XMonad.Actions.Promote (promote)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import           XMonad.Actions.WindowGo (runOrRaiseNext)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Prompt.AppLauncher (launchApp)
import           XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad (xmonadPromptC)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run (spawnPipe)

import           System.Directory (getDirectoryContents, getHomeDirectory)
import           System.FilePath (takeBaseName, combine)
import           XMonad.Prompt
import           System.Posix.Env (getEnv)

------------------------------------------------------------------------
-- Password section
--

type PromptLabel = String
data Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = liftM passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (mkComplFunFromList passwords) passwordFunction

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = mkPassPrompt "Generate password" generatePassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword passLabel = spawn $ "pass --clip " ++ passLabel

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = spawn $ "pass generate --force " ++ passLabel ++ " 30"

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword passLabel = spawn $ "pass rm --force " ++ passLabel

-- | Retrieve the list of passwords from the password storage 'passwordStoreDir
--
getPasswords :: String -> IO [String]
getPasswords passwordStoreDir = liftM (map takeBaseName) $ getDirectoryContents passwordStoreDir

--
-- End Password section
------------------------------------------------------------------------

-- | The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "urxvt"

myTerminalQuery :: Query Bool
myTerminalQuery = className =? "URxvt"

-- | My preferential browser
--
myBrowser :: String
myBrowser = "firefox"

myBrowserQuery :: Query Bool
myBrowserQuery = className =? "Firefox"

-- | My preferential emacs
--
myEmacsQuery :: Query Bool
myEmacsQuery = appName =? "emacs"

-- | Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- | Whether a mouse click select the focus or is just passed to the window
--
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- | Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth  = 2

-- | modMask lets you specify which modkey you want to use. mod4mask is window key
-- I'm used to prefix key because of emacs, stumpwm, conkeror and firefox with keysnail
--
myModMask :: KeyMask
myModMask = mod4Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- | Prefix key
--
prefixKey :: String
prefixKey = "C-;"

-- | A utility function to compute the final binding with the prefix key
--
prefix :: String -> String
prefix = ((prefixKey ++ " ") ++)

-- | Execute a command and show its result in a zenity window dialog
--
spawnZenityCmd :: String -> X ()
spawnZenityCmd = spawn . zenityCmd
                 where zenityCmd :: String -> String
                       zenityCmd cmd = "zenity --info --text \"$(" ++ cmd ++ ")\""

-- | Display some data in a zenity window dialog
--
-- spawnZenityText :: String -> X ()
-- spawnZenityText = spawn . zenityText
--                   where zenityText :: String -> String
--                         zenityText s = "zenity --info --text '" ++ s ++ "'"

-- | Run or raise with a default config folder from which finding the command
--
myRunOrRaise :: String -> String -> Query Bool -> X ()
myRunOrRaise home cmd = runOrRaiseNext $ combine home cmd

-- | My keymap as (prefix keybindings, command description, command)
--
myKeymapWithDescription :: String -> XConfig Layout -> [(String, String, X ())]
myKeymapWithDescription home conf @(XConfig { terminal   = myTerm
                                            , layoutHook = myLayoutHook
                                            , workspaces = myWss}) =
  [ (prefix "C-g"       , "abort"                      , spawn "xdotool key Escape")
  , (prefix "e"         , "emacs"                      , myRunOrRaise home "bin/emacs/emacs.sh"                        myEmacsQuery)
  , (prefix "S-x"       , "xephyr"                     , myRunOrRaise home "bin/xephyr/xephyr-stumpwm.sh"              (className =? "Xephyr"))
  , (prefix "y"         , "yed"                        , myRunOrRaise home "bin/app/yed.sh"                            (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-install4j-runtime-launcher-Launcher"))
  , (prefix "S-c"       , "lighttable"                 , myRunOrRaise home "applications/LightTable/LightTable"        (className =? "ltbin"))
  , (prefix "i"         , "ide"                        , myRunOrRaise home "bin/ide/idea.sh"                           (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "jetbrains-idea-ce"))
  , (prefix "C-x"       , "ide"                        , myRunOrRaise home "bin/ide/idea.sh"                           (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "jetbrains-idea-ce"))
  , (prefix "S-j"       , "visualvm"                   , myRunOrRaise home "applications/visualvm/bin/visualvm"        (className =? "java-lang-Thread"))
  , (prefix "S-l"       , "sqldeveloper"               , myRunOrRaise home "applications/sqldeveloper/sqldeveloper.sh" (appName =? "sun-awt-X11-XFramePeer"))
  , (prefix "S-g"       , "gparted"                    , myRunOrRaise home "bin/admin/gparted.sh"                      (appName =? "gpartedbin" <&&> className =? "Gpartedbin"))
  , (prefix "S-u"       , "usb-creator-gtk"            , myRunOrRaise home "bin/admin/usb-creator.sh"                  (appName =? "usb-creator-gtk" <&&> className =? "Usb-creator-gtk"))
  , (prefix prefixKey   , "promote"                    , promote)
  , (prefix "x"         , "terminal"                   , runOrRaiseNext myTerm                     myTerminalQuery)
  , (prefix "S-s"       , "desktop-settings"           , runOrRaiseNext "cinnamon-settings"        (className =? "Cinnamon-settings.py"))
  , (prefix "S-t"       , "video-player"               , runOrRaiseNext "totem"                    (className =? "Totem"))
  , (prefix "C-e"       , "pdf-reader"                 , runOrRaiseNext "evince"                   (className =? "Evince" <||> className =? ".evince-wrapped"))
  , (prefix "C-i"       , "image-viewer"               , runOrRaiseNext "eog"                      (className =? "Eog"))
  , (prefix "d"         , "pinta"                      , runOrRaiseNext "pinta"                    (className =? "Pinta"))
  , (prefix "S-i"       , "gimp"                       , runOrRaiseNext "gimp"                     (className =? "Gimp"))
  , (prefix "C-a"       , "music-reader"               , runOrRaiseNext "audacious"                (className =? "Audacious"))
  , (prefix "M1-j"      , "jconcole"                   , runOrRaiseNext "jconsole"                 (className =? "sun-tools-jconsole-JConsole"))
  , (prefix "C-c"       , "arduino-ide"                , runOrRaiseNext "arduino"                  (className =? "processing-appBase"))
  , (prefix "C-w"       , "wireshark"                  , runOrRaiseNext "gksudo wireshark"         (className =? "wireshark"))
  , (prefix "n"         , "nautilus"                   , runOrRaiseNext "nautilus"                 (appName =? "nautilus" <&&> className =? "Nautilus"))
  , (prefix "S-n"       , "thunar"                     , runOrRaiseNext "thunar"                   (className =? "thunar"))
  , (prefix "C-M1-f"    , "filezilla"                  , runOrRaiseNext "filezilla"                (className =? "Filezilla"))
  , (prefix "C-v"       , "virtualbox"                 , runOrRaiseNext "virtualbox"               (appName =? "Virtualbox" <&&> className =? "Qt-subapplication"))
  , (prefix "u"         , "unetbootin"                 , runOrRaiseNext "unetbootin"               (className =? "unetbootin"))
  , (prefix "/"         , "transmission"               , runOrRaiseNext "transmission-gtk" $       (appName =? "transmission-gtk" <&&> className =? "Transmission-gtk") <||> (appName =? ".transmission-gtk-wrapped" <&&> className =? ".transmission-gtk-wrapped"))
  , (prefix "S-g"       , "gparted"                    , runOrRaiseNext "gksudo /usr/sbin/gparted" (className =? "gpartedbin"))
  , (prefix "S-f"       , "file-progression"           , runOrRaiseNext ""                         (className =? "file_progress"))
  , (prefix "C-S-x"     , "xosview"                    , runOrRaiseNext "xosview"                  (className =? "xosview"))
  , (prefix "b"         , "baobab"                     , runOrRaiseNext "baobab"                   (appName =? "baobab" <&&> className =? "Baobab"))
  , (prefix "z"         , "gitk"                       , runOrRaiseNext "gitk"                     (className =? "gitk"))
  , (prefix "S-f"       , "fbreader"                   , runOrRaiseNext "fbreader"                 (className =? "fbreader"))
  , (prefix "M1-t"      , "tuxguitar"                  , runOrRaiseNext "tuxguitar"                (className =? "TuxGuitar"))
  , (prefix "C-c"       , "skype"                      , runOrRaiseNext "skype"                    (appName =? "skype" <&&> className =? "Skype"))
  , (prefix "f"         , "browser"                    , runOrRaiseNext myBrowser                  myBrowserQuery)
  , (prefix "C-S-e"     , "env"                        , spawnZenityCmd "env")
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
  , (prefix "C-S-s"     , "suspend"                    , spawn "systemctl suspend")
  , (prefix "C-S-h"     , "hibernate"                  , spawn "systemctl hibernate")
  , (prefix "S-a"       , "ssh-add"                    , spawn "~/bin/ssh/ssh-add.sh")
  , (prefix "C-b"       , "brightness-decrease"        , spawn "~/bin/brightness/dec-brightness.sh 10")
  , (prefix "C-f"       , "brightness-increase"        , spawn "~/bin/brightness/inc-brightness.sh 10")
  , (prefix "C-S-m"     , "brightness-half"            , spawn "~/bin/brightness/half-brightness.sh")
  , (prefix "S-m"       , "brightness-max"             , spawn "~/bin/brightness/max-brightness.sh")
  , (prefix "M1-f"      , "sound-increase"             , spawn "exec amixer set Master 10%+")
  , (prefix "M1-b"      , "sound-decrease"             , spawn "exec amixer set Master 10%-")
  , (prefix "M1-m"      , "sound-toggle"               , spawn "exec amixer set Master toggle")
  , (prefix "C-o"       , "wifi-off"                   , spawn "~/bin/wifi/wifi-off.sh")
  , (prefix "S-o"       , "wifi-on"                    , spawn "~/bin/wifi/wifi-on.sh")
  , (prefix "C-M1-l"    , "session-lock"               , spawn "~/bin/session/lock.sh")
  , (prefix "M1-e"      , "evince-prompt"              , launchApp myXPConfig "evince")
  , (prefix "s"         , "search-url"                 , search)
  , (prefix "r"         , "exec"                       , runOrRaisePrompt myXPConfig)
  , (prefix "g"         , "goto"                       , windowPromptGoto myXPConfig)
  , (prefix "M1-x"      , "meta-x"                     , xmonadPromptC keymapDescription myXPConfig)
  , (prefix "p"         , "pass-read"                  , passPrompt myXPConfig)
  , (prefix "C-p"       , "pass-generate"              , passGeneratePrompt myXPConfig)
  , (prefix "C-S-p"     , "pass-generate"              , passRemovePrompt myXPConfig)
  , (prefix "c"         , "close-current-window"       , kill >> spawn "notify-send 'window closed!'")
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
  , (prefix "S-q"       , "xmonad-restart"             , spawn "xmonad --restart && notify-send 'XMonad Restarted!'")
  , (prefix "M1-q"      , "xmonad-quit"                , io exitSuccess)] ++
  -- M1-n   - Switch to workspace with id n
  -- S-n    - Move the client to workspace with id n
  -- C-M1-n - Switch to the workspace with id n
  [(prefix $ pk ++ k, desc ++ k , windows $ f i) | (i, k) <- zip myWss $ map show [1 .. length myWss]
                                                 , (f, pk, desc) <- [ (W.view      , "M1-"  , "workspace-keep-screen-switch-to-id-")
                                                                    , (W.shift     , "S-"   , "workspace-move-client-to-id-")
                                                                    , (W.greedyView, "C-M1-", "workspace-switch-to-id-")]]
  where -- Permits the search through the system browser
        searchSite = S.promptSearchBrowser myXPConfig myBrowser
        search     = SM.submap . mkKeymap conf $
                     [ ("g"  , searchSite S.google)
                     , ("h"  , searchSite S.hoogle)
                     , ("S-h", searchSite S.hackage)
                     , ("a"  , searchSite S.amazon)
                     , ("i"  , searchSite S.imdb)
                     , ("y"  , searchSite S.youtube)
                     , ("w"  , searchSite S.wikipedia)
                     , ("d"  , searchSite $ S.searchEngine "duckduckgo" "https://duckduckgo.com/?t=lm&q=")]
        -- Rework the keymap description to extract the command description and the associated actions
        keymapDescription = map (\ (keybinding, xmonadActionDesc, xmonadAction) -> (xmonadActionDesc ++ " - " ++ keybinding, xmonadAction)) fullKeymap
        fullKeymap = myKeymapWithDescription home conf

-- | Key bindings
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf = mkKeymap conf keymap
                   where keymap = map (\ (keybinding, _, xmonadAction) -> (keybinding, xmonadAction)) fullKeymap
                         fullKeymap = myKeymapWithDescription home conf

-- | Mouse bindings: default actions bound to mouse events
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

-- | My own layout
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

------------------------------------------------------------------------
-- Workspaces:

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

workspaceEmacs, workspaceTerminal, workspaceWeb, workspaceCode, workspaceIrc, workspaceIde, workspaceFloat, workspaceBooks, workspaceDb :: String
workspaceEmacs    = "1:emacs"
workspaceTerminal = "2:terminal"
workspaceWeb      = "3:web"
workspaceCode     = "4:code"
workspaceIrc      = "5:irc"
workspaceIde      = "6:ide"
workspaceFloat    = "7:ide"
workspaceBooks    = "8:books"
workspaceDb       = "9:db"

myWorkspaces :: [String]
myWorkspaces = [ workspaceEmacs
               , workspaceTerminal
               , workspaceWeb
               , workspaceCode
               , workspaceIrc
               , workspaceIde
               , workspaceFloat
               , workspaceBooks
               , workspaceDb]

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
    [ className =? "MPlayer"                                                    --> doShift workspaceFloat >> doFloat
    , className =? "Gimp"                                                       --> doShift workspaceFloat >> doFloat
    , className =? "Zenity"                                                     --> doFloat
    , appName   =? "desktop_window"                                             --> doIgnore
    , appName   =? "kdesktop"                                                   --> doIgnore
    , myEmacsQuery                                                              --> doShift workspaceEmacs
    , myTerminalQuery                                                           --> doShift workspaceTerminal
    , myBrowserQuery                                                            --> doShift workspaceWeb
    , className =? "Evince" <||> className =? ".evince-wrapped"                 --> doShift workspaceBooks
    , appName =? "sun-awt-X11-XFramePeer" <&&> className =? "jetbrains-idea-ce" --> doShift workspaceIde
    , appName =? "sun-awt-X11-XFramePeer"                                       --> doShift workspaceDb
    , className =? "Skype"                                                      --> doShift workspaceIrc]

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

-- | Font
--
myDefaultFont :: String
myDefaultFont = "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"

-- | Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#1e2320"
myFocusedBorderColor :: String
myFocusedBorderColor = "#5193f7"

-- | Default configuration for prompt
--
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
              { font              = myDefaultFont
              , bgColor           = "#1e2320"
              , fgColor           = "#dddddd"
              , bgHLight          = "#5f5f5f"
              , fgHLight          = "#ffffff"
              , borderColor       = "#ffffff"
              , height            = 20
              , position          = Top
              , historySize       = 256
              , promptBorderWidth = 1}

-- | Now run xmonad with all the defaults we set up.
main :: IO ()
main = do
  home <- getHomeDirectory
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig {
                  terminal           = myTerminal
                , focusFollowsMouse  = myFocusFollowsMouse
                , clickJustFocuses   = myClickJustFocuses
                , borderWidth        = myBorderWidth
                , modMask            = myModMask
                , workspaces         = myWorkspaces
                , normalBorderColor  = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys               = myKeys home
                , mouseBindings      = myMouseBindings
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
