{ pkgs }:

{
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "MonoLisa:size=10";
        dpi-aware = "no";
        include = "${pkgs.foot.src}/themes/dracula";
      };
    };
  };

}
