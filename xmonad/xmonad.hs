import XMonad
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenFull)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import XMonad.Hooks.EwmhDesktops (ewmh)
import Graphics.X11.Xlib.Display
import System.IO

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaultConfig
        { 
          borderWidth        = 2
        , normalBorderColor  = "black"
        , focusedBorderColor = "orange"
 	, workspaces         = myWorkspaces
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = myHandleEventHook
        , logHook            = dynamicLogWithPP xmobarPP 
                                 { 
                                    ppOutput = hPutStrLn xmproc
                                  , ppTitle  = xmobarColor "green" "" . shorten 125
                                  , ppHidden = removeNSP
                                 }
        } `additionalKeys` myKeys

myWorkspaces = ["www", "term", "irc", "music", "video", "gaming"] ++ map show [7..9]

myManageHook = manageDocks <+> composeAll
    [
      className =? "Firefox"              --> doShift "www",
      className =? "Tor Browser"          --> doShift "www",
      className =? "Spotify"              --> doShift "music",
      className =? "Hexchat"	          --> doShift "irc",
      className =? "Gnome-terminal"	  --> doShift "term",
      className =? "vlc"	          --> doShift "video" <+> doFullFloat,
      className =? "Steam"		  --> doShift "gaming",
      isFullscreen                        --> doFullFloat,
      isDialog                            --> doCenterFloat
    ] <+> scratchpadManageHookDefault <+> manageHook defaultConfig

myLayoutHook = lessBorders OnlyFloat $ myLayout

myLayout = avoidStruts ((spacing' 12 $ tiled) ||| (spacing' 50 $ Full)) ||| noBorders (fullscreenFull Full)
  where
     tiled   = ResizableTall nmaster delta ratio []
     nmaster = 1
     ratio   = 1/2
     delta   = 5/100

myKeys = [ 
          ((mod1Mask, xK_Page_Down), spawn ".xmonad/bin/lang-change")
         ,((mod1Mask, xK_Insert), (scratchpadSpawnActionTerminal "urxvt"))
         ,((mod1Mask, xK_a), sendMessage MirrorExpand)
         ,((mod1Mask, xK_z), sendMessage MirrorShrink)
         ] ++ multimediaKeys

-- combines Layout.Gaps for outer spacing and Layout.Spacing for inner spacing
spacing' :: Int -> l a -> ModifiedLayout Spacing (ModifiedLayout Gaps l) a
spacing' x = spacing x . gaps [(U,x),(D,x),(R,x),(L,x)]

-- docksEventHook fixes windows hiding xmobar on workspace 1
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> handleEventHook defaultConfig

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

multimediaKeys = [
          ((0, xK_XF86AudioPlay), spawn "sp play")
         ,((0, xK_XF86AudioPrev), spawn "sp prev")
         ,((0, xK_XF86AudioNext), spawn "sp next")
         ,((0, xK_XF86AudioMute), spawn ".xmonad/bin/volume toggle")
         ,((0, xK_XF86AudioLowerVolume), spawn ".xmonad/bin/volume down")
         ,((0, xK_XF86AudioRaiseVolume), spawn ".xmonad/bin/volume up")
	]

