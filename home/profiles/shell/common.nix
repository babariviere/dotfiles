{ pkgs, ...}:

{
  home.packages = with pkgs; [ dogdns sd fd ripgrep ];
}
