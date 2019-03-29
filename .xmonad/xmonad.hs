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
import XMonad.Layout.Gaps
import Data.Monoid
import System.Exit
import XMonad.Util.Run
import System.IO
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.CycleWS


myBorderWidth   = 1

myWorkspaces = ["1:web", "2:code", "3:files"] ++ map show [4..9]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)
        , ((modm .|. shiftMask, xK_t), setLayout $ XMonad.layoutHook conf)
    ]

newKeys x = myKeys x `M.union` keys defaultConfig x

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

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

myManageHook = composeAll . concat $
    [
        -- Applications config
        [ className =? "Google-chrome" --> viewShift "1:web"]
        , [ className =? "Sublime_text" --> viewShift "2:code"]
        , [className =? "Nautilus" --> viewShift "3:files"]
        --
        , [className =? "Gnome-calculator" --> doFloat]
       -- , [className =? "Gnome-calculator" --> doFloat]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        
-- Define default layouts used on most workspaces
defaultLayouts = smartBorders(avoidStruts(
    ResizableTall 1 (3/100) (1/2) []
    ||| noBorders Full
    ||| ThreeColMid 1 (3/100) (3/4)
    ))

-- Define layout for specific workspaces
-- webLayout = noBorders $ Full
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Sink the window back to the grid.
    , ((modm, button2), (\w -> withFocused $ windows . W.sink))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]


myConfig = defaultConfig {
    modMask = mod4Mask,
    terminal = "gnome-terminal",
    borderWidth = myBorderWidth,
    workspaces = myWorkspaces,
    keys = newKeys,
    manageHook = myManageHook,
    layoutHook = defaultLayouts, 
    handleEventHook = fullscreenEventHook,
    mouseBindings = myMouseBindings
    }
    `additionalKeys`
    [
        ((mod4Mask, xK_F2), spawn "amixer set Master 2- unmute")
        , ((mod4Mask, xK_F3), spawn "amixer set Master 2+ unmute")
        --
        , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((mod4Mask, xK_g), spawn "google-chrome &")
        , ((mod4Mask, xK_s), spawn "subl &")
        , ((mod4Mask, xK_f), spawn "nautilus &")
        --
        , ((mod4Mask, xK_k), sendMessage $ MirrorExpand)
        , ((mod4Mask, xK_j), sendMessage $ MirrorShrink)
        , ((mod4Mask, xK_h), sendMessage $ Shrink)
        , ((mod4Mask, xK_l), sendMessage $ Expand)
        
    ]
