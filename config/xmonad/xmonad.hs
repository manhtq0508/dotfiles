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
	$ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
	$ myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
	, terminal = "kitty"
	, startupHook = myStartupHook
    }
  `additionalKeysP`
    [ ("M-f"  , spawn "firefox"                   )
	, ("M-p", spawn "polybar-msg cmd restart")
	, ("C-S-<Space>", spawn "~/.config/rofi/launchers/type-1/launcher.sh")
	, ("M-S-s", spawn "ksnip -r")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 -1%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 +1%")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "Blueman-manager" --> doFloat
    , isDialog            --> doFloat
    ]

myStartupHook :: X ()
myStartupHook = do 
    spawnOnce "~/.config/xmonad/startup.sh"

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
