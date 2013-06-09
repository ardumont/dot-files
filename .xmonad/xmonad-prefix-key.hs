import XMonad
import XMonad.Actions.Submap
import XMonad.Util.Paste
import Data.Map as M
import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad defaultConfig {
     XMonad.keys = prefixKey myPrefix,
     XMonad.modMask = 0,
     XMonad.mouseBindings = myMouseBindings
  }

prefixKey :: (Ord t1, Ord t) => (t, t1) -> XConfig Layout -> Map (t, t1) (X ())
--prefixKey pfx@(modifier, keycode) x =
prefixKey pfx x =
  M.fromList $
     let oldKeys = XMonad.keys defaultConfig x
         mine = M.fromList (myKeys x)
         merged = M.union oldKeys mine
     in
        [ (pfx, submap merged) ]

myMouseMod :: KeyMask
myMouseMod = mod4Mask

myPrefix :: (KeyMask, KeySym)
myPrefix = (0, xK_Escape)

myKeys :: t -> [((KeyMask, KeySym), X ())]
myKeys _ = [ (myPrefix, sendKey (fst myPrefix) (snd myPrefix)) ]

myMouseBindings :: t -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ = M.fromList $
       -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((myMouseMod, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((myMouseMod, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((myMouseMod, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
