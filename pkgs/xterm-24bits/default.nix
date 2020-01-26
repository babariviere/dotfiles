{ runCommand, ncurses }:

runCommand "xterm-24bits" { preferLocalBuild = true; } ''
  mkdir -p $out/share/terminfo
  ${ncurses}/bin/tic -x -o $out/share/terminfo ${
    <config/terminfo/xterm-24bits.src>
  }
''
