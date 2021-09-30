{ pkgs }:

{
  programs.foot = {
    enable = true;
    # Broken for me
    server.enable = false;
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
