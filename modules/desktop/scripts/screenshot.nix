{ writeShellScriptBin, maim, libnotify }:

writeShellScriptBin "screenshot" ''
  filename=$(date '+%Y-%m-%d-%H-%M-%S').png
  file=$HOME/Pictures/screenshot/$filename
  mkdir -p $(dirname $file)
  ${maim}/bin/maim $@ $file && ${libnotify}/bin/notify-send "Screenshot taken" "saved into $file"
''
