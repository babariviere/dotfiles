import XMonad

import qualified XMonad.StackSet as W

import XMonad.Actions.EasyMotion
  ( ChordKeys(..)
  , EasyMotionConfig(..)
  , proportional
  , selectWindow
  )

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane

import XMonad.Prompt
import XMonad.Prompt.Ssh

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import qualified XMonad.Layout.LayoutModifier

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myFont :: Int -> String
myFont size = "xft:Biosevka-" ++ show size

myLayout :: Choose Tall (Choose TwoPane Full) a
myLayout = tiled ||| TwoPane delta ratio ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isDialog --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myStartupHook :: X ()
myStartupHook = do
  spawn "pkill trayer"

  spawn "feh --bg-scale $HOME/Pictures/backgrounds"
  spawn "sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --tint 0x000000 --height 20 --transparent true --alpha 0 --monitor primary"

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  -- launching and killing programs
  [ ((myModMask,               xK_d), spawn "rofi -show drun")
  , ((myModMask .|. shiftMask, xK_d), spawn "rofi -show run")
  , ((myModMask .|. shiftMask, xK_k), kill) -- %! Close the focused window

  -- focus
  , ((myModMask,               xK_n), windows W.focusDown) -- focus next window
  , ((myModMask,               xK_p), windows W.focusUp)   -- focus previous window
  , ((myModMask .|. shiftMask, xK_n), windows W.swapDown) -- focus next window
  , ((myModMask .|. shiftMask, xK_p), windows W.swapUp)   -- focus previous window
  , ((myModMask,               xK_a), selectWindow emConf >>= (`whenJust` windows . W.focusWindow))

  -- prompt
  , ((myModMask .|. shiftMask, xK_s), sshPrompt promptConf)

  , ((myModMask,               xK_End), spawn "xlock -mode swarm")
  ]
  ++
  -- mod-{z,x,c} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{z,x,c} %! Move client to screen 1, 2, or 3
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_z, xK_x, xK_c] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  where
    emConf =
      def
        { sKeys = AnyKeys [xK_a, xK_r, xK_s, xK_t, xK_g]
        , overlayF = proportional (0.1 :: Double)
        , emFont = myFont 25
        }
    promptConf =
      def
      { font = myFont 12
      , bgColor = "#000000"
      , fgColor = "#ffffff"
      , bgHLight = "#ffffff"
      , fgHLight = "#000000"
      , promptBorderWidth = 2
      , height = 30
      , position = Top
      }

myXmobarPP :: ScreenId -> PP
myXmobarPP sid =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, _, _, l, wins] -> [ws, l, wins]
    , ppExtras = [logLayoutOnScreen sid, logTitlesOnScreen sid formatFocused formatUnfocused]
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

myConfig :: XConfig
  (XMonad.Layout.LayoutModifier.ModifiedLayout
     SmartBorder (Choose Tall (Choose TwoPane Full)))
myConfig =
  def
    { modMask = myModMask

    , layoutHook = smartBorders myLayout
    , manageHook = myManageHook
    , startupHook = myStartupHook

    , focusFollowsMouse = False
    , clickJustFocuses = False
    , terminal = myTerminal
    } `additionalKeys` myKeys
