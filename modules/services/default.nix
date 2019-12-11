{ config, lib, pkgs, ... }:

{
  imports = [
    ./bitwarden.nix
    ./fwupd.nix
    ./gpg.nix
    ./keyring.nix
    ./mail.nix
    ./ssh.nix
    ./syncthing.nix
  ];
}
