import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
	 . docks
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = avoidStruts 
	$ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
	$ myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
	, terminal = "kitty"
    , startupHook = spawnOnce "~/.config/polybar/update_layout.py"
    }
  `additionalKeysP`
    [ ("M-f"  , spawn "firefox"                   )
	, ("M-p", spawn "polybar-msg cmd restart")
	, ("C-<space>", spawn "rofi -show drun")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
