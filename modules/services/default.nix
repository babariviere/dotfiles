{ config, lib, pkgs, ... }:

{
  imports = [ ./bitwarden.nix ./fwupd.nix ./gpg.nix ./ssh.nix ./syncthing.nix ];
}
