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
import XMonad.Util.Scratchpad
import System.IO


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
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
      className =? "vlc"	          --> doShift "video",
      className =? "Steam"		  --> doShift "gaming",
      isFullscreen                        --> doFullFloat,
      isDialog                            --> doCenterFloat
    ] <+> scratchpadManageHookDefault <+> manageHook defaultConfig

myLayoutHook = lessBorders OnlyFloat $ avoidStruts $ spacing 5 $ layoutHook defaultConfig

myKeys = [ 
          ((mod1Mask, xK_Page_Down), spawn "~/dotfiles/xmonad/bin/lang-change")
         ,((mod1Mask, xK_Insert), (scratchpadSpawnActionTerminal "urxvt"))
         ]


-- docksEventHook fixes windows hiding xmobar on workspace 1
myHandleEventHook = docksEventHook <+> handleEventHook defaultConfig

-- Filters-out the NSP workspace from xmobar list
removeNSP :: WorkspaceId -> String
removeNSP "NSP" = ""
removeNSP name  = name
