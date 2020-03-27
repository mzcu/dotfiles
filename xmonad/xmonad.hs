import XMonad
import XMonad.Actions.OnScreen
import XMonad.Layout
import XMonad.Layout.Column
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenFull)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.DynamicProperty
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops (ewmh)
import Data.List
import Graphics.X11.Xlib.Display
import System.IO
import qualified Data.Map as M

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaultConfig
        { 
          modMask            = mod4Mask
        , borderWidth        = 1
        , normalBorderColor  = "black"
        , focusedBorderColor = "gray"
        , terminal           = myTerminal
        , keys               = \conf -> myKeys conf `M.union` keys defaultConfig conf
 	, workspaces         = myWorkspaces
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = myHandleEventHook
        , logHook            = dynamicLogWithPP xmobarPP 
                                 { 
                                    ppOutput = hPutStrLn xmproc
                                  , ppTitle  = xmobarColor "white" "" . shorten 125
                                  , ppHidden = removeNSP
                                  , ppCurrent = xmobarColor "#f8f8f8" "#655c82" . wrap " *" "  "
                                  , ppVisible = xmobarColor "#292929" "grey" . wrap " -" "  "
                                 }
        }

myTerminal = ".xmonad/bin/st -e tmux"

myWorkspaces = ["main", "aux1", "aux2", "org", "video", "gaming"] ++ map show [7..8] ++ ["dashboard"]

myManageHook = manageDocks <+> composeAll
    [
      className =? "vlc"	          --> doShift "video" <+> doFullFloat,
      className =? "Steam"		  --> doShift "gaming",
      isFullscreen                        --> doFullFloat,
      isDialog                            --> doCenterFloat
    ] <+> scratchpadManageHookDefault <+> manageHook defaultConfig

myLayoutHook = lessBorders OnlyFloat $ onWorkspace "dashboard" portraitLayout myLayout


portraitLayout = spacing' 12 $ Column 1.6

myLayout = withSpacing ||| noBorders (fullscreenFull Full)
  where
     withSpacing = avoidStruts (tiled ||| (spacing' 25 $ Full))
     tiled   = ResizableTall nmaster delta ratio []
     nmaster = 1
     ratio   = 1/2
     delta   = 5/100


-- combines Layout.Gaps for outer spacing and Layout.Spacing for inner spacing
spacing' :: Int -> l a -> ModifiedLayout Spacing (ModifiedLayout Gaps l) a
spacing' x = spacing x . gaps [(U,x),(D,x),(R,x),(L,x)]

-- docksEventHook fixes windows hiding xmobar on workspace 1
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> dynamicWindowPropertyHook <+> handleEventHook defaultConfig

dynamicWindowPropertyHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> doShift "dashboard")

-- Filters-out the NSP workspace from xmobar list
removeNSP :: WorkspaceId -> String
removeNSP "NSP" = ""
removeNSP name  = name

-- Multimedia keys
xK_XF86AudioLowerVolume	= 0x1008ff11
xK_XF86AudioMute       	= 0x1008ff12
xK_XF86AudioRaiseVolume	= 0x1008ff13
xK_XF86AudioPlay       	= 0x1008ff14
xK_XF86AudioPrev       	= 0x1008ff16
xK_XF86AudioNext       	= 0x1008ff17

myKeys conf @ XConfig { XMonad.modMask = modMask } = M.fromList $
-- custom mappings
    [ 
          ((modMask, xK_Page_Down), spawn ".xmonad/bin/lang-change")
         ,((modMask, xK_Insert), scratchpadSpawnActionCustom ".xmonad/bin/st -n scratchpad -e tmux")
         ,((modMask, xK_a), sendMessage MirrorExpand)
         ,((modMask, xK_z), sendMessage MirrorShrink)
         ,((modMask .|. shiftMask, xK_l), spawn "slock")
    ] ++
-- multimedia keys
    [
          ((0, xK_XF86AudioPlay), spawn "sp play")
         ,((0, xK_XF86AudioPrev), spawn "sp prev")
         ,((0, xK_XF86AudioNext), spawn "sp next")
         ,((0, xK_XF86AudioMute), spawn ".xmonad/bin/volume toggle")
         ,((0, xK_XF86AudioLowerVolume), spawn ".xmonad/bin/volume down")
         ,((0, xK_XF86AudioRaiseVolume), spawn ".xmonad/bin/volume up")
    ]
-- multi-display keys customization
 ++  [ 
	((m .|. modMask, k), windows (f i))
		| (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
     		, (f, m) <- [ (viewOnScreen 0, 0)
                 		 , (viewOnScreen 1, controlMask) ]
   ]
