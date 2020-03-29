{ writeShellScriptBin, maim, libnotify, xdg-user-dirs }:

writeShellScriptBin "screenshot" ''
  filename=$(date '+%Y-%m-%d-%H-%M-%S').png
  file=$(${xdg-user-dirs}/bin/xdg-user-dir PICTURES)/screenshot/$filename
  mkdir -p $(dirname $file)
  ${maim}/bin/maim $@ $file && ${libnotify}/bin/notify-send "Screenshot taken" "saved into $file"
''
