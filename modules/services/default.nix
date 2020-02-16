{ config, lib, pkgs, ... }:

{
  imports = [
    ./bitwarden.nix
    ./fwupd.nix
    ./gpg.nix
    ./keyring.nix
    ./mail.nix
    ./postgres.nix
    ./ssh.nix
    ./syncthing.nix
    ./virtualbox.nix
    ./zerotier.nix
  ];
}
