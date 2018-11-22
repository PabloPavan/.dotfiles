import XMonad
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import Data.Monoid
import System.Exit
import XMonad.Util.Run
import System.IO

import Control.Monad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS


-- See https://github.com/davidbrewer/xmonad-ubuntu-conf

-- Width of the window border in pixels.
--
myBorderWidth   = 1

--
myWorkspaces = ["web", "code"] ++ map show [3..9]

--



------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)
        --, ("<Next>", spawn "xbacklight -10")
        -- , ((modm, xF86XK_MonBrightnessDown) spawn "xbacklight +10")
        --, ((mod4Mask, xK_Right), spawn "amixer set Master 5%- unmute &")
        --, ((mod4Mask, xK_Left), spawn "amixer set Master 5%+ unmute &")
    ]

--myKeys =
--    -- volume key binding
--    [
--        ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
--        , ("M-<Left>", prevWS)
--        , ("M-<Right>", nextWS)
--    --, ("<XF86AudioLowerVolume>", spawn "aumix2pipe -10")
--    ]

newKeys x = myKeys x `M.union` keys defaultConfig x

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

{-
Xmobar configuration variables. These settings control the appearance
of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor = "#eeeeee" -- color of window title
myTitleLength = 80 -- truncate window title to this length
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft = "{" -- wrap urgent workspace with these
myUrgentWSRight = "}"


-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP {
-- ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
, ppCurrent = xmobarColor myCurrentWSColor ""
. wrap myCurrentWSLeft myCurrentWSRight
, ppVisible = xmobarColor myVisibleWSColor ""
. wrap myVisibleWSLeft myVisibleWSRight
, ppUrgent = xmobarColor myUrgentWSColor ""
. wrap myUrgentWSLeft myUrgentWSRight
}

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)



-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

myManageHook = composeAll . concat $
    [
          -- Applications that go to web
        [ className =? "Google-chrome" --> viewShift "web"]
        , [ className =? "Sublime_text" --> viewShift "code"]
        , [className =? "Gnome-calculator" --> doFloat]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift


-- Define default layouts used on most workspaces
defaultLayouts = smartBorders(avoidStruts(
    ResizableTall 1 (3/100) (1/2) []
    -- ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
    ||| noBorders Full
    -- ||| Grid
    ||| ThreeColMid 1 (3/100) (3/4)
    -- ||| Circle
    ))

-- Define layout for specific workspaces
webLayout = noBorders $ Full

-- Put all layouts together
--myLayouts = onWorkspace "web" webLayout $ defaultLayouts


-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig {
    modMask = mod4Mask,
    --terminal = "xterm -bg black -fg gray -fa \"Ubuntu Mono\" -fs 12 -e '/usr/bin/env -u TMUX tmux -2'",
    terminal = "gnome-terminal",
    borderWidth = myBorderWidth,
    workspaces = myWorkspaces,
    keys = newKeys,
    manageHook = myManageHook,
    handleEventHook = fullscreenEventHook
    -- border colors from solarized dark palette
    --focusedBorderColor = "#268bd2",
    --normalBorderColor = "#586e75",
    --layoutHook = myLayouts

    -- manageHook = composeAll [
    -- className =? "Gimp" --> doFloat,
    -- className =? "MPlayer" --> doFloat,
    -- isFullscreen --> doFullFloat
    -- ]
    }
    `additionalKeys`
    [
        --((mod4Mask, xK_R), spawn "amixer set Master 5- unmute")
        --, ((mod4Mask, xK_L), spawn "amixer set Master 5+ unmute")

        ---- XF86XK_AudioLowerVolume
        ((shiftMask, xK_F2), spawn "amixer set Master 2- unmute")

        ---- XF86XK_AudioRaiseVolume
        , ((shiftMask, xK_F3), spawn "amixer set Master 2+ unmute")


       -- , ((mod1Mask .|. shiftMask, xK_Up), spawn "xbacklight -inc 5")
       -- , ((mod1Mask .|. shiftMask, xK_Down), spawn "xbacklight -dec 5")

        --, ((mod4Mask, xK_l), spawn "systemctl suspend")
        , ((mod4Mask, xK_l), spawn "xscreensaver-command --lock")
        --, ((mod4Mask, xK_t), spawn "./.xmonad/touchpadHandler.sh")
        , ((mod4Mask, xK_g), spawn "google-chrome &")
        , ((mod4Mask, xK_s), spawn "subl &")
        --, ((mod4Mask .|. shiftMask, xK_q), spawn "systemctl poweroff")


        -- Pas très fonctionnel
        --, ((mod4Mask, xK_c), spawn "source ~/proxy/citeu.sh")
        --, ((mod4Mask, xK_m), spawn "source ~/proxy/maison.sh")
        --, ((mod4Mask, xK_i), spawn "source ~/proxy/iut.sh")

        --((0x0, 122), spawn "amixer set Master 2- unmute")
        --, ((0x0, 123), spawn "amixer set Master 2+ unmute")
        --, ((0x0, 232), spawn "xbacklight -inc 40000")
        --, ((0x0, 233), spawn "xbacklight -dec 40000")


        --((0, 0x1008FF11), spawn "amixer set Master 2-"),
        --((0, 0x1008FF13), spawn "amixer set Master 2+"),


        --, ((mod1Mask, Next), spawn "xbacklight -10")
        --, ((mod1Mask, XF86MonBrightnessDown), spawn "xbacklight +10")
    ]
