import XMonad

import XMonad.Actions.NoBorders
import XMonad.Actions.TiledWindowDragging
import XMonad.Layout.DraggingVisualizer

import XMonad.Actions.FloatKeys
import qualified XMonad.Actions.FlexibleResize as Flex

import Data.Ratio ((%))

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile

import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.Minimize
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = minimize . BW.boringWindows
    $ draggingVisualizer
    $ avoidStruts 
    $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
    $ myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , terminal = "kitty"
    , startupHook = myStartupHook
    }
  `additionalKeysP`
    [ ("M-f"  , spawn "firefox"                   )
    , ("M-p", spawn "polybar-msg cmd restart")
    , ("M-S-p", spawn "polybar-msg cmd toggle")
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindow)
    , ("M-g", withFocused toggleBorder)
    , ("C-S-<Space>", spawn "~/.config/rofi/launcher.sh")
    , ("M-S-s", spawn "ksnip -r")
    , ("M-S-f", spawn "ksnip -m && notify-send -u normal 'Ksnip' 'Full screen captured'")
    , ("M-S-l", spawn "betterlockscreen -l blur --off 60")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 -1%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 +1%")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 5%+")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
    , ("M-a", sendMessage MirrorShrink)
    , ("M-z", sendMessage MirrorExpand)
    , ("M-c", withFocused (keysMoveWindowTo (683,384) (1%2,1%2)))
    , ("M-<Left>", withFocused (keysMoveWindow (-20,0)))
    , ("M-<Right>", withFocused (keysMoveWindow (20,0)))
    , ("M-<Up>", withFocused (keysMoveWindow (0,-20)))
    , ("M-<Down>", withFocused (keysMoveWindow (0,20)))
    , ("M-S-<Left>", withFocused (keysResizeWindow (-20,0) (0,0)))
    , ("M-S-<Right>", withFocused (keysResizeWindow (20,0) (0,0)))
    , ("M-S-<Up>", withFocused (keysResizeWindow (0,-20) (0,0)))
    , ("M-S-<Down>", withFocused (keysResizeWindow (0,20) (0,0)))
    ]
  `additionalMouseBindings`
    [ ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    , ((mod4Mask .|. shiftMask, button1), dragWindow)
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "Blueman-manager" --> doFloat
    , className =? "pavucontrol" --> doFloat
    , className =? "Conky" --> doIgnore
    , isDialog            --> doFloat
    ]

myStartupHook :: X ()
myStartupHook = do 
    spawnOnce "~/.local/bin/startup.sh"

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    tiled    = ResizableTall nmaster delta ratio []
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
