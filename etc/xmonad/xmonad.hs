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
myTerminal = "alacritty"

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook :: ManageHook
myManageHook = composeAll [isDialog --> doFloat]

myStartupHook = do
  spawn "pkill trayer"

  spawn "feh --bg-scale $HOME/Pictures/backgrounds"
  spawn "sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --tint 0x000000 --height 20 --transparent true --alpha 0"

myKeys =
  [ ((myModMask,               xK_p), spawn "rofi -show drun")
  , ((myModMask .|. shiftMask, xK_p), spawn "rofi -show run")
  , ((myModMask,               xK_t), spawn myTerminal)
  ]

myXmobarPP :: ScreenId -> PP
myXmobarPP sid =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [(logTitlesOnScreen sid) formatFocused formatUnfocused]
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
    white = xmobarColor "#ffffff" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner screen@(S sid)
  | sid < 2 =
    pure $
    statusBarPropTo
      ("_XMONAD_LOG_" <> show sid)
      ("xmobar -x " <> show sid <> " ~/.config/xmobar/xmobarrc" <> show sid) $
    pure (myXmobarPP screen)
  | otherwise = mempty

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
    , startupHook = myStartupHook

    , focusFollowsMouse = False
    , clickJustFocuses = False
    , terminal = myTerminal
    } `additionalKeys` myKeys
