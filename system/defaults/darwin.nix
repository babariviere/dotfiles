{ config, pkgs, ... }:

{
  nix.sandboxPaths = [
    "/System/Library/Frameworks"
    "/System/Library/PrivateFrameworks"
    "/usr/lib"
    "/private/tmp"
    "/private/var/tmp"
    "/usr/bin/env"
  ];
  services.nix-daemon = {
    enable = true;
    logFile = "/var/log/nix-daemon.log";
  };
}
