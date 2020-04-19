{ config, lib, pkgs, ... }:

{
  imports = [
    ./bitwarden.nix
    ./fwupd.nix
    ./gpg.nix
    ./guix.nix
    ./keyring.nix
    ./libvirtd.nix
    ./mail.nix
    ./postgres.nix
    ./ssh.nix
    ./syncthing.nix
    ./virtualbox.nix
    ./zerotier.nix
  ];
}
