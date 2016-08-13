import XMonad
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { 
          borderWidth        = 2
        , normalBorderColor  = "black"
        , focusedBorderColor = "orange"
 	, workspaces = ["www", "term", "irc", "music", "video", "gaming"] ++ map show [7..9]
        , manageHook         = manageDocks <+> myManageHook
        , layoutHook         = lessBorders OnlyFloat $ avoidStruts $ myLayoutHook $ layoutHook defaultConfig
        -- , modMask = mod4Mask
        -- fixes windows hiding xmobar on workspace 1
        , handleEventHook = docksEventHook <+> handleEventHook defaultConfig
        , logHook            = dynamicLogWithPP xmobarPP 
                                 { 
                                    ppOutput = hPutStrLn xmproc
                                  , ppTitle = xmobarColor "green" "" . shorten 75
                                 }
        }


myManageHook = composeAll
    [
      className =? "Firefox"              --> doShift "www",
      className =? "Tor Browser"          --> doShift "www",
      className =? "Spotify"              --> doShift "music",
      className =? "Hexchat"	          --> doShift "irc",
      className =? "Gnome-terminal"	  --> doShift "term",
      className =? "vlc"	          --> doShift "video",
      className =? "Steam"		  --> doShift "gaming",
      isFullscreen                        --> doFullFloat,
      isDialog                            --> doCenterFloat
    ] <+> manageHook defaultConfig

myLayoutHook = spacing 5
