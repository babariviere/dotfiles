import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Ungrab

myModMask = mod4Mask

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook :: ManageHook
myManageHook = composeAll [isDialog --> doFloat]

myKeys = [((myModMask, xK_p), spawn "rofi -show drun")]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow
    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw .
      (\w ->
         if null w
           then "untitled"
           else w) .
      shorten 30
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S sid) = pure $ statusBarProp ("xmobar -x " <> show sid <> " ~/.config/xmobar/xmobarrc") $ pure myXmobarPP

main :: IO ()
main =
  xmonad
  . ewmhFullscreen
  . ewmh
  . dynamicEasySBs barSpawner
  $ myConfig

myConfig =
  def
    { modMask = myModMask
    , layoutHook = myLayout
    , manageHook = myManageHook
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , terminal = "alacritty"
    } `additionalKeys` myKeys
