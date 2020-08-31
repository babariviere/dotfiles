-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html

import Xmobar

import Data.List

config :: Config
config =
  defaultConfig
    { font = "xft:Mononoki Nerd Font:pixelsize=12:antialias=true:hinting=true",
      additionalFonts = ["xft:FontAwesome:pixelsize=13"],
      bgColor = "#282c34",
      fgColor = "#abb2bf",
      position = Top,
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      persistent = True,
      iconRoot = "./xpm",
      commands =
        [ -- Trayer padding
          Run $ Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 10,
          -- Time and date
          Run $ Date "\xf133 %b %d %Y (%H:%M)" "date" 50,
          -- Network up and down
          Run $ Network "wlan0" ["-t", "\xf0aa <tx>kb  \xf0ab <rx>kb"] 20,
          -- Cpu usage in percent
          Run $ Cpu ["-t", "\xf108  cpu: (<total>%)"] 20,
          -- Ram used number and percent
          Run $ Memory ["-t", "\xf233  mem: <used>M (<usedratio>%)"] 20,
          -- Runs a standard shell command 'uname -r' to get kernel version
          Run $ Com "uname" ["-r"] "" 36000,
          -- Battery
          Run $
            BatteryP
              ["BAT0"]
              ["-t", "\xf240  bat: <left>%"]
              600,
          -- Volume
          Run $
            Volume
              "default"
              "Master"
              [ "-t",
                "<status> vol: <volume>%",
                "--",
                "-O",
                "\x2022",
                "-C",
                "#98c379",
                "-o",
                "\x2022",
                "-c",
                "#e06c75"
              ]
              10,
          -- Prints out the left side items such as workspaces, layout, etc.
          -- The workspaces are 'clickable' in my configs.
          Run UnsafeStdinReader
        ],
      sepChar = "%",
      alignSep = "}{",
      template =
        let fc color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"
            templateParts =
              [ " <icon=haskell_20.xpm/>",
                " %UnsafeStdinReader% }{ " ++ fc "#b3afc2" "\xf17c %uname%",
                fc "#d19a66" "%battery%",
                "%default:Master%",
                fc "#e5c07b" "%cpu%",
                fc "#e06c75" "%memory%",
                fc "#98c379" "%wlan0%",
                fc "#61afef" "%date%",
                "%trayerpad%"
              ]
            separator = " <fc=#666666>|</fc> "
         in concat $ Data.List.intersperse separator templateParts
    }

main :: IO ()
main = xmobar config
