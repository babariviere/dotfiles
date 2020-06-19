------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------

-- Base
import Control.Arrow (first)
-- Data
import Data.Char (isSpace)
import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid
import Data.Tree
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS
  ( WSType (..),
    nextScreen,
    prevScreen,
  )
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Hooks
import XMonad.Hooks.DynamicLog
  ( PP (..),
    dynamicLogWithPP,
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (..),
    avoidStruts,
    docksEventHook,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
-- Layouts
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle ((??), EOT (EOT), mkToggle, single)
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers (MIRROR, NBFULL, NOBORDERS),
  )
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T
  ( ToggleLayout (Toggle),
    toggleLayouts,
  )
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:pixelsize=13"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty" -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"

myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormColor :: String
myNormColor = "#292d3e" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#bbc5ff" -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "@setWallpaper@ &"
  -- spawnOnce "picom &"
  spawnOnce
    "trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint @trayerBackground@ --height 18 &"
  -- spawnOnce "kak -d -s mysession &"
  setWMName "Baba"

------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
  colorRangeFromClassName
    (0x29, 0x2d, 0x3e) -- lowest inactive bg
    (0x29, 0x2d, 0x3e) -- highest inactive bg
    (0xc7, 0x92, 0xea) -- active bg
    (0xc0, 0xa7, 0x9a) -- inactive fg
    (0x29, 0x2d, 0x3e) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig _colorizer =
  (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 40,
      gs_cellwidth = 250,
      gs_cellpadding = 8,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where
    conf = def

-- Set favorite apps for the spawnSelected'
myAppGrid :: [(String, String)]
myAppGrid =
  [ ("Audacity", "audacity"),
    ("Deadbeef", "deadbeef"),
    ("Emacs", "emacs"),
    ("Firefox", "firefox"),
    ("Geany", "geany"),
    ("Geary", "geary"),
    ("Gimp", "gimp"),
    ("Kdenlive", "kdenlive"),
    ("LibreOffice Impress", "loimpress"),
    ("LibreOffice Writer", "lowriter"),
    ("OBS", "obs"),
    ("PCManFM", "pcmanfm"),
    ("Simple Terminal", "st"),
    ("Steam", "steam"),
    ("Surf Browser", "surf suckless.org"),
    ("Xonotic", "xonotic-glx")
  ]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
myXTConfig :: XPConfig
myXTConfig =
  def
    { font = "xft:Mononoki Nerd Font:size=9",
      bgColor = "@background@",
      fgColor = "@foreground@",
      bgHLight = "@lightBlack@",
      fgHLight = "@foregroundBold@",
      borderColor = "@lightWhite@",
      promptBorderWidth = 0,
      promptKeymap = myXPKeymap,
      position = Top,
      --    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      height = 20,
      historySize = 256,
      historyFilter = id,
      defaultText = [],
      autoComplete = Just 100000, -- set Just 100000 for .1 sec
      showCompletionOnTab = False,
      searchPredicate = isPrefixOf,
      alwaysHighlight = True,
      maxComplRows = Nothing -- set to Just 5 for 5 rows
    }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
myXTConfig' :: XPConfig
myXTConfig' = myXTConfig {autoComplete = Nothing}

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList =
  [ ( "m",
      manPrompt
    ), -- manpages prompt
    ( "p",
      passPrompt
    ), -- get passwords (requires 'pass')
    ( "g",
      passGeneratePrompt
    ), -- generate passwords (requires 'pass')
    ( "r",
      passRemovePrompt
    ), -- remove passwords (requires 'pass')
    ( "s",
      sshPrompt
    ), -- ssh prompt
    ("x", xmonadPrompt) -- xmonad prompt
  ]

------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like key bindings)
------------------------------------------------------------------------
myXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
myXPKeymap =
  M.fromList $
    map
      (first $ (,) controlMask) -- control + <key>
      [ ( xK_z,
          killBefore
        ), -- kill line backwards
        ( xK_k,
          killAfter
        ), -- kill line fowards
        ( xK_a,
          startOfLine
        ), -- move to the beginning of the line
        ( xK_e,
          endOfLine
        ), -- move to the end of the line
        ( xK_m,
          deleteString Next
        ), -- delete a character foward
        ( xK_b,
          moveCursor Prev
        ), -- move cursor forward
        ( xK_f,
          moveCursor Next
        ), -- move cursor backward
        ( xK_BackSpace,
          killWord Prev
        ), -- kill the previous word
        ( xK_y,
          pasteString
        ), -- paste a string
        ( xK_g,
          quit
        ), -- quit out of prompt
        (xK_bracketleft, quit)
      ]
      ++ map
        (first $ (,) altMask) -- meta key + <key>
        [ ( xK_BackSpace,
            killWord Prev
          ), -- kill the prev word
          ( xK_f,
            moveWord Next
          ), -- move a word forward
          ( xK_b,
            moveWord Prev
          ), -- move a word backward
          ( xK_d,
            killWord Next
          ), -- kill the next word
          ( xK_n,
            moveHistory W.focusUp'
          ), -- move up thru history
          (xK_p, moveHistory W.focusDown') -- move down thru history
        ]
      ++ map
        (first $ (,) 0) -- <key>
        [ (xK_Return, setSuccess True >> setDone True),
          (xK_KP_Enter, setSuccess True >> setDone True),
          (xK_BackSpace, deleteString Prev),
          (xK_Delete, deleteString Next),
          (xK_Left, moveCursor Prev),
          (xK_Right, moveCursor Next),
          (xK_Home, startOfLine),
          (xK_End, endOfLine),
          (xK_Down, moveHistory W.focusUp'),
          (xK_Up, moveHistory W.focusDown'),
          (xK_Escape, quit)
        ]

------------------------------------------------------------------------
-- SEARCH ENGINES
------------------------------------------------------------------------
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
nixos, reddit, urban :: S.SearchEngine
nixos = S.searchEngine "nixos" "https://nixos.org/nixos/options.html#"
reddit = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban =
  S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList =
  [ ("d", S.duckduckgo),
    ("g", S.google),
    ("h", S.hoogle),
    ("i", S.images),
    ("n", nixos),
    ("r", reddit),
    ("s", S.stackage),
    ("t", S.thesaurus),
    ("v", S.vocabulary),
    ("b", S.wayback),
    ("u", urban),
    ("w", S.wikipedia),
    ("y", S.youtube),
    ("z", S.amazon)
  ]

------------------------------------------------------------------------
-- TREE SELECT
------------------------------------------------------------------------
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction _tsDefaultConfig =
  TS.treeselectAction
    tsDefaultConfig
    [ Node (TS.TSNode "hello" "displays hello" (spawn "xmessage hello!")) [],
      Node (TS.TSNode "shutdown" "poweroff the system" (spawn "shutdown")) [],
      Node
        (TS.TSNode "xmonad" "working with xmonad" (return ()))
        [ Node
            ( TS.TSNode
                "edit xmonad"
                "edit xmonad"
                (spawn (myTerminal ++ " -e vim ~/.xmonad/xmonad.hs"))
            )
            [],
          Node
            ( TS.TSNode
                "recompile xmonad"
                "recompile xmonad"
                (spawn "xmonad --recompile")
            )
            [],
          Node
            (TS.TSNode "restart xmonad" "restart xmonad" (spawn "xmonad --restart"))
            []
        ],
      Node
        (TS.TSNode "system monitors" "system monitoring applications" (return ()))
        [ Node
            (TS.TSNode "htop" "a much better top" (spawn (myTerminal ++ " -e htop")))
            [],
          Node
            ( TS.TSNode
                "glances"
                "an eye on your system"
                (spawn (myTerminal ++ " -e glances"))
            )
            [],
          Node
            ( TS.TSNode
                "gtop"
                "a more graphical top"
                (spawn (myTerminal ++ " -e gtop"))
            )
            [],
          Node
            (TS.TSNode "nmon" "network monitor" (spawn (myTerminal ++ " -e nmon")))
            [],
          Node
            ( TS.TSNode
                "s-tui"
                "stress your system"
                (spawn (myTerminal ++ " -e s-tui"))
            )
            []
        ]
    ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig =
  TS.TSConfig
    { TS.ts_hidechildren = True,
      TS.ts_background = 0xdd292d3e,
      TS.ts_font = "xft:Mononoki Nerd Font:bold:pixelsize=13",
      TS.ts_node = (0xffd0d0d0, 0xff202331),
      TS.ts_nodealt = (0xffd0d0d0, 0xff292d3e),
      TS.ts_highlight = (0xffffffff, 0xff755999),
      TS.ts_extra = 0xffd0d0d0,
      TS.ts_node_width = 200,
      TS.ts_node_height = 20,
      TS.ts_originX = 0,
      TS.ts_originY = 0,
      TS.ts_indent = 80,
      TS.ts_navigate = myTreeNavigation
    }

myTreeNavigation =
  M.fromList
    [ ((0, xK_Escape), TS.cancel),
      ((0, xK_Return), TS.select),
      ((0, xK_space), TS.select),
      ((0, xK_Up), TS.movePrev),
      ((0, xK_Down), TS.moveNext),
      ((0, xK_Left), TS.moveParent),
      ((0, xK_Right), TS.moveChild),
      ((0, xK_k), TS.movePrev),
      ((0, xK_j), TS.moveNext),
      ((0, xK_h), TS.moveParent),
      ((0, xK_l), TS.moveChild),
      ((0, xK_o), TS.moveHistBack),
      ((0, xK_i), TS.moveHistForward)
    ]

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
  [ ("M-C-r", spawn "xmonad --recompile"), -- Recompiles xmonad
    ("M-S-r", spawn "xmonad --restart"), -- Restarts xmonad
    ("M-S-q", io exitSuccess), -- Quits xmonad

    -- Open my preferred terminal and the FISH shell.
    ("M-<Return>", spawn (myTerminal)),
    -- Open firefox
    ("M-w", spawn (myBrowser)),
    -- Open emacs
    ("M-e", spawn "emacseditor"),
    -- Run Prompt
    ("M-d", shellPrompt myXTConfig), -- Shell Prompt

    -- Windows
    ("M-S-c", kill1), -- Kill the currently focused client
    ("M-S-a", killAll), -- Kill all windows on current workspace

    -- Floating windows
    ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
    ("M-<Delete>", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-S-<Delete>", sinkAll), -- Push ALL floating windows to tile

    -- Grid Select
    ("C-g g", spawnSelected' myAppGrid), -- grid select favorite apps
    ("C-g t", goToSelected $ mygridConfig myColorizer), -- goto selected
    ("C-g b", bringSelected $ mygridConfig myColorizer), -- bring selected

    -- Tree Select
    ("M-S-t", treeselectAction tsDefaultConfig), -- tree select actions menu

    -- Windows navigation
    ("M-m", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
      --, ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
    ("M-S-j", windows W.swapDown), -- Swap focused window with next window
    ("M-S-k", windows W.swapUp), -- Swap focused window with prev window
    ("M-<Backspace>", promote), -- Moves focused window to master, others maintain order
    ("M1-S-<Tab>", rotSlavesDown), -- Rotate all windows except master and keep focus in place
    ("M1-C-<Tab>", rotAllDown), -- Rotate all the windows in the current stack
    --, ("M-S-s", windows copyToAll)
    ("M-C-s", killAllOtherCopies),
    -- Layouts
    ("M-<Tab>", sendMessage NextLayout), -- Switch to next layout
    ("M-C-M1-<Up>", sendMessage Arrange),
    ("M-C-M1-<Down>", sendMessage DeArrange),
    ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full
    ("M-S-<Space>", sendMessage ToggleStruts), -- Toggles struts
    ("M-S-n", sendMessage $ MT.Toggle NOBORDERS), -- Toggles noborder
    ("M-<KP_Multiply>", sendMessage (IncMasterN 1)), -- Increase number of clients in master pane
    ("M-<KP_Divide>", sendMessage (IncMasterN (-1))), -- Decrease number of clients in master pane
    ("M-S-<KP_Multiply>", increaseLimit), -- Increase number of windows
    ("M-S-<KP_Divide>", decreaseLimit), -- Decrease number of windows
    ("M-h", sendMessage Shrink), -- Shrink horiz window width
    ("M-l", sendMessage Expand), -- Expand horiz window width
    ("M-C-j", sendMessage MirrorShrink), -- Shrink vert window width
    ("M-C-k", sendMessage MirrorExpand), -- Exoand vert window width

    -- Workspaces
    ("M-.", nextScreen), -- Switch focus to next monitor
    ("M-,", prevScreen), -- Switch focus to prev monitor

    -- Scratchpads
    ("M-S-<Return>", namedScratchpadAction myScratchPads "terminal"),
    -- Multimedia Keys
    ("<XF86AudioMute>",   spawn "pamixer -t"),
    ("<XF86AudioLowerVolume>", spawn "pamixer -d 3"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer -i 3"),
    ("<XF86MonBrightnessUp>", spawn "light -A 5"),
    ("<XF86MonBrightnessDown>", spawn "light -U 5"),
    ("<Print>", spawn "flameshot gui")
  ]
    -- Appending search engines to keybindings list
    ++ [("M-s " ++ k, S.promptSearch myXTConfig' f) | (k, f) <- searchList]
    ++ [("M-S-s " ++ k, S.selectSearch f) | (k, f) <- searchList]
    ++ [("M-p " ++ k, f myXTConfig') | (k, f) <- promptList]

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces =
  clickable
    . map xmobarEscape
    $ ["dev", "www", "chat", "mus", "sys", "doc", "vbox", "vid", "gfx"]
  where
    clickable l =
      [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>"
        | (i, ws) <- zip [1 .. 9] l,
          let n = i
      ]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title
-- of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
    -- I'm doing it this way because otherwise I would have to write out
    -- the full name of my clickable workspaces, which would look like:
    -- doShift "<action xdotool super+8>gfx</action>"
    [ className =? "obs" --> doShift (myWorkspaces !! 7),
      title =? "Firefox" --> doShift (myWorkspaces !! 1),
      title =? "qutebrowser" --> doShift (myWorkspaces !! 1),
      className =? "mpv" --> doShift (myWorkspaces !! 7),
      className =? "vlc" --> doShift (myWorkspaces !! 7),
      className =? "Gimp" --> doShift (myWorkspaces !! 8),
      className =? "Gimp" --> doFloat,
      title =? "Oracle VM VirtualBox Manager" --> doFloat,
      className =? "Oracle VM VirtualBox Manager"
        --> doShift
          (myWorkspaces !! 4),
      (className =? "firefox" <&&> resource =? "Dialog") --> doFloat, -- Float Firefox Dialog
      title =? "Discord" --> doShift (myWorkspaces !! 2)
    ]
    <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing ::
  Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' ::
  Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall =
  renamed [Replace "tall"] $ limitWindows 12 $ mySpacing 8 $
    ResizableTall
      1
      (3 / 100)
      (1 / 2)
      []

magnify =
  renamed [Replace "magnify"]
    $ magnifier
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle = renamed [Replace "monocle"] $ limitWindows 20 $ Full

floats = renamed [Replace "floats"] $ limitWindows 20 $ simplestFloat

grid =
  renamed [Replace "grid"]
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

spirals = renamed [Replace "spirals"] $ mySpacing' 8 $ spiral (6 / 7)

threeCol =
  renamed [Replace "threeCol"] $ limitWindows 7 $ mySpacing' 4 $
    ThreeCol
      1
      (3 / 100)
      (1 / 2)

threeRow =
  renamed [Replace "threeRow"]
    $ limitWindows 7
    $ mySpacing' 4
    -- Mirror takes a layout and rotates it by 90 degrees.
    -- So we are applying Mirror to the ThreeCol layout.
    $ Mirror
    $ ThreeCol 1 (3 / 100) (1 / 2)

tabs =
  renamed [Replace "tabs"]
  -- I cannot add spacing to this layout because it will
  -- add spacing between window and tabs which looks bad.
  $
    tabbed shrinkText myTabConfig
  where
    myTabConfig =
      def
        { fontName = "xft:Mononoki Nerd Font:regular:pixelsize=11",
          -- TODO: fix colors
          activeColor = "#292d3e",
          inactiveColor = "#3e445e",
          activeBorderColor = "#292d3e",
          inactiveBorderColor = "#292d3e",
          activeTextColor = "#ffffff",
          inactiveTextColor = "#d0d0d0"
        }

-- The layout hook
myLayoutHook =
  avoidStruts
    $ mouseResize
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
    $ myDefaultLayout
  where
    -- I've commented out the layouts I don't use.
    myDefaultLayout =
      tall
        ||| magnify
        ||| noBorders monocle
        ||| floats
        ||| noBorders tabs

-- grid
-- spirals
-- threeCol
-- threeRow

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  -- the xmonad, ya know...what the WM is named after!
  xmonad $
    ewmh
      def
        { manageHook =
            (isFullscreen --> doFullFloat) <+> myManageHook <+> manageDocks,
          -- Run xmonad commands from command line with "xmonadctl command". Commands include:
          -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
          -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
          -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
          handleEventHook =
            serverModeEventHookCmd
              <+> serverModeEventHook
              <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
              <+> docksEventHook,
          modMask = myModMask,
          terminal = myTerminal,
          startupHook        = myStartupHook,
          layoutHook = myLayoutHook,
          workspaces = myWorkspaces,
          borderWidth = myBorderWidth,
          normalBorderColor = myNormColor,
          focusedBorderColor = myFocusColor,
          focusFollowsMouse = False,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = \x ->
                    hPutStrLn xmproc x,
                  ppCurrent = xmobarColor "@green@" "" . wrap "[" "]", -- Current workspace in xmobar
                  ppVisible = xmobarColor "@green@" "", -- Visible but not current workspace
                  ppHidden = xmobarColor "@blue@" "" . wrap "*" "", -- Hidden workspaces in xmobar
                  ppHiddenNoWindows = xmobarColor "@red@" "", -- Hidden workspaces (no windows)
                  ppTitle = xmobarColor "@foreground@" "" . shorten 60, -- Title of active window in xmobar
                  ppSep = "<fc=#666666>|</fc>", -- Separators in xmobar
                  ppUrgent = xmobarColor "@lightRed@" "" . wrap "!" "!", -- Urgent workspace
                  ppExtras = [windowCount], -- # of windows current workspace
                  ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                }
        }
      `additionalKeysP` myKeys
