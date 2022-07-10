{
  programs.alacritty = {
    enable = true;
    settings = {
      env.WINIT_X11_SCALE_FACTOR = "1.0";
      font = {
        normal.family = "Biosevka";
        size = 10;
      };
      colors = {
        primary = {
          background = "#000000";
          foreground = "#ffffff";
        };
        normal = {
          black   = "#000000";
          red     = "#ff8059";
          green   = "#44bc44";
          yellow  = "#d0bc00";
          blue    = "#2fafff";
          magenta = "#feacd0";
          cyan    = "#00d3d0";
          white   = "#bfbfbf";
        };
        bright = {
          black   = "#595959";
          red     = "#ef8b50";
          green   = "#70b900";
          yellow  = "#c0c530";
          blue    = "#79a8ff";
          magenta = "#b6a0ff";
          cyan    = "#6ae4b9";
          white   = "#ffffff";
        };
      };
    };
  };
}
