import XMonad

-- Toggle border
import XMonad.Actions.NoBorders 

-- Drag and tiled window
import XMonad.Actions.TiledWindowDragging
import XMonad.Layout.DraggingVisualizer

-- Move and resize floating window
import XMonad.Actions.FloatKeys

-- Resize floating window at any corner
import qualified XMonad.Actions.FlexibleResize as Flex

-- Operator %
import Data.Ratio ((%))

-- Give space for statusbar
import XMonad.Hooks.ManageDocks

-- Actions for each type of window
import XMonad.Hooks.ManageHelpers

-- Used to map keys and mouse
import XMonad.Util.EZConfig

-- Run command at startup
import XMonad.Util.SpawnOnce

-- Layout
import XMonad.Layout.ResizableTile      -- Resizable Tall
import XMonad.Layout.Grid               -- Grid
import XMonad.Layout.ThreeColumns       -- ThreeCol
import XMonad.Layout.Magnifier          -- Zoom
import XMonad.Layout.MouseResizableTile -- Mouse Tall

-- Gap between windows
import XMonad.Layout.Spacing

-- Hide useless focus border
import XMonad.Layout.NoBorders

-- Remember size and workspace window therein
import XMonad.Hooks.EwmhDesktops

-- Maximize and minimize window
import XMonad.Actions.Minimize
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     $ myConfig

myWorkspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- ===================== Config =====================
myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout 
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    , terminal = "kitty"
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 2
    , normalBorderColor = "#A89984"  -- Gray
    , focusedBorderColor = "#FF0000" -- Red
    , workspaces = myWorkspaces'
    }
  `additionalKeysP` myKeyBindings
  `additionalMouseBindings` myMouseBindings
-- ===================== Config =====================


-- ===================== ManageHook =====================
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"            --> doCenterFloat
    , className =? "Blueman-manager" --> doCenterFloat
    , className =? "pavucontrol"     --> doCenterFloat
    , className =? "Conky"           --> doIgnore
    , isDialog                       --> doFloat
    ]
-- ===================== ManageHook =====================


-- ===================== StartupHook =====================
myStartupHook :: X ()
myStartupHook = do 
    spawnOnce "~/.local/bin/startup.sh"
    spawnOnce "notify-send -u low 'Dunst' 'Welcome'"
-- ===================== StartupHook =====================


-- ===================== LayoutHook =====================
myLayout = minimize
         $ BW.boringWindows
         $ draggingVisualizer
         $ avoidStruts 
         $ spacingRaw True (Border 0 0 0 0) True (Border 3 3 3 3) True
         $ myCustomLayout

myCustomLayout = smartBorders (tiled ||| Mirror tiled ||| Grid ||| noBorders Full ||| threeCol ||| mouseResizableTile ||| mouseResizableTileMirrored)
  where
    tiled    = ResizableTall nmaster delta ratio []
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
-- ===================== LayoutHook =====================


-- ===================== Key and mouse bindings =====================
myKeyBindings :: [(String, X ())]
myKeyBindings = 
    [ 
    -- Quick actions
      ("M-f"  , spawn "firefox")
    , ("M-S-s", spawn "ksnip -r")
    , ("M-S-f", spawn "ksnip -m && notify-send -u low 'Ksnip' 'Full screen captured'")
    , ("C-S-<Space>", spawn "~/.config/rofi/launcher.sh")

    -- Polybar
    , ("M-p", spawn "polybar-msg cmd restart")
    , ("M-S-p", spawn "polybar-msg cmd toggle")

    --  Min, max window
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindow)

    -- Toggle border
    , ("M-g", withFocused toggleBorder)

    -- Lockscreen
    , ("M-S-l", spawn "betterlockscreen -l blur --off 60")
    
    -- Fn Button (brightness, volume, media control)
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 -1%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 +1%")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 5%+")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")

    -- Resize in layout Tall
    , ("M-z", sendMessage MirrorShrink)
    , ("M-a", sendMessage MirrorExpand)

    -- Move and resize floating window
    , ("M-c", withFocused (keysMoveWindowTo (683,384) (1%2,1%2)))
    , ("M-<Left>", withFocused (keysMoveWindow (-20,0)))
    , ("M-<Right>", withFocused (keysMoveWindow (20,0)))
    , ("M-<Up>", withFocused (keysMoveWindow (0,-20)))
    , ("M-<Down>", withFocused (keysMoveWindow (0,20)))
    , ("M-S-<Left>", withFocused (keysResizeWindow (-20,0) (0,0)))
    , ("M-S-<Right>", withFocused (keysResizeWindow (20,0) (0,0)))
    , ("M-S-<Up>", withFocused (keysResizeWindow (0,-20) (0,0)))
    , ("M-S-<Down>", withFocused (keysResizeWindow (0,20) (0,0)))
    
    -- Resize in Mouse Tall
    , ("M-u", sendMessage ExpandSlave)
    , ("M-i", sendMessage ShrinkSlave)
    ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings = 
    [ ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    , ((mod4Mask .|. shiftMask, button1), dragWindow)
    ]
-- ===================== Key and mouse bindings =====================
