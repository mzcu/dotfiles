import XMonad
import XMonad.Actions.CycleWS (moveTo, WSType (NonEmptyWS, WSIs), Direction1D (Next, Prev))
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
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.DebugStack
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops (ewmh)
import Data.List
import Data.Maybe (isNothing, isJust)
import Graphics.X11.Xlib.Display
import System.IO
import qualified Data.Map as M

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks def
        { 
          modMask            = mod4Mask
        , borderWidth        = 1
        , normalBorderColor  = "black"
        , focusedBorderColor = "gray"
        , terminal           = myTerminal
        , keys               = \conf -> myKeys conf `M.union` keys def conf
        , workspaces         = myWorkspaces
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = myHandleEventHook
        , logHook            = dynamicLogWithPP xmobarPP 
                                 { 
                                    ppOutput = hPutStrLn xmproc
                                  , ppTitle  = xmobarColor "white" "" . shorten 125
                                  , ppHidden = removeNSP
                                  , ppCurrent = xmobarColor "#f8f8f8" "#02b3e4" . wrap " *" "  "
                                  , ppVisible = xmobarColor "#292929" "grey" . wrap " -" "  "
                                 }
        , startupHook        = myStartupHook
        }


myStartupHook = windows (viewOnScreen 1 "dashboard") >> windows (viewOnScreen 0 "main")

myTerminal = "bin/st -e tmux"

myWorkspaces = ["main", "aux1", "aux2", "org", "video", "gaming"] ++ map show [7..8] ++ ["dashboard"]

myManageHook = manageDocks <+> composeAll
    [
      className =? "vlc"                  --> doShift "video" <+> doFullFloat,
      className =? "Steam"		  --> doShift "gaming",
      isFullscreen                        --> doFullFloat,
      isDialog                            --> doCenterFloat
    ] <+> scratchpadManageHookDefault <+> manageHook def

myLayoutHook = lessBorders OnlyScreenFloat $ onWorkspace "dashboard" portraitLayout myLayout


portraitLayout = spacing' 12 $ Column 1.6

myLayout = withSpacing ||| noBorders (fullscreenFull Full)
  where
     withSpacing = avoidStruts (tiled ||| multiColumn ||| (gaps [(L,500),(R,500),(U,200),(D,200)] Full))
     tiled   = ResizableTall nmaster delta ratio []
     multiColumn = ThreeColMid 1 (3/100) (1/3)
     nmaster = 1
     ratio   = 1/2
     delta   = 5/100


-- combines Layout.Gaps for outer spacing and Layout.Spacing for inner spacing
spacing' :: Int -> l a -> ModifiedLayout Spacing (ModifiedLayout Gaps l) a
spacing' x = spacing x . gaps [(U,x),(D,x),(R,x),(L,x)]

myHandleEventHook = fullscreenEventHook <+> dynamicWindowPropertyHook <+> handleEventHook def

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
xK_XF86HomePage         = 0x1008ff18
xK_XF86Launch5          = 0x1008ff45


isNormalVisibleWS wisp = let 
                            t = W.tag wisp
                            s = W.stack wisp
                            in not ("NSP" `isPrefixOf` t) && isJust s


-- spawn for local binaries which are not on system path
spawn' :: MonadIO m => String -> m ()
spawn' s = spawn $ ".config/xmonad/" ++ s


myKeys conf @ XConfig { XMonad.modMask = modMask } = M.fromList $
-- custom mappings
    [ 
          ((modMask, xK_Page_Down), spawn' "bin/lang-change")
         ,((controlMask, xK_grave), scratchpadSpawnActionCustom "bin/st -n scratchpad -e tmux")
         ,((modMask, xK_a), sendMessage MirrorExpand)
         ,((modMask, xK_z), sendMessage MirrorShrink)
         ,((modMask .|. shiftMask, xK_l), spawn "slock & sleep 1 && xset dpms force off")
         ,((modMask, xK_bracketleft), XMonad.Actions.CycleWS.moveTo Prev (WSIs $ return isNormalVisibleWS))
         ,((modMask, xK_bracketright), XMonad.Actions.CycleWS.moveTo Next (WSIs $ return isNormalVisibleWS))
         ,((modMask, xK_f), spawn "files")
         ,((modMask, xK_o), spawn "brave-browser")
    ] ++
-- multimedia keys
    [
          ((0, xK_XF86AudioPlay), spawn "sp play")
         ,((0, xK_XF86AudioPrev), spawn "sp prev")
         ,((0, xK_XF86AudioNext), spawn "sp next")
         ,((0, xK_XF86AudioMute), spawn' "bin/volume toggle")
         ,((0, xK_XF86AudioLowerVolume), spawn' "bin/volume down")
         ,((0, xK_XF86AudioRaiseVolume), spawn' "bin/volume up")
         ,((0, xK_XF86Launch5), spawn "lights")
    ]
-- multi-display keys customization described
-- source: https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Actions-OnScreen.html
-- modkey + 1-0: Switch to workspace 1-0 on screen 0
-- modkey + control + 1-0: Switch to workspace 1-0 on screen 1
-- modkey + control + shift + 1-0: Default greedyView behaviour
 ++  [ 
	((m .|. modMask, k), windows (f i)) | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
                                            , (f, m) <- [   (viewOnScreen 0, 0)
                 	                                    ,(viewOnScreen 1, controlMask)
                                                            ,(W.greedyView, controlMask .|. shiftMask) 
                                                        ]
    ]
-- experiments
 ++ [ ((controlMask, xK_l), trace "test")
    ]
