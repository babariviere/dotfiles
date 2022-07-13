{ pkgs }:

{
  services.picom = {
    enable = true;
    package = pkgs.picom-next;
    backend = "glx";
    vSync = true;
    extraOptions = ''
      unredir-if-possible = false;
    '';
  };
}
