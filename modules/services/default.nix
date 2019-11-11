{ config, lib, pkgs, ... }:

{
  imports = [ ./gpg.nix ./ssh.nix ./syncthing.nix ];
}
