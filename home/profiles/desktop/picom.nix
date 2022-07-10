{
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    extraOptions = ''
      unredir-if-possible = true;
    '';
  };
}
