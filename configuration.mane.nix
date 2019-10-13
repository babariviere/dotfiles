{ config, lib, pkgs, ... }:

let
  user = "babariviere";
in
{
  imports = [
    ./.

    ./modules/desktop
    ./modules/dev/rust.nix
    ./modules/editors/emacs.nix
    ./modules/services/syncthing.nix
    ./modules/shell/direnv.nix
    ./modules/shell/git.nix
    ./modules/shell/zsh.nix
    ./modules/social/discord.nix
  ];

  services.dotfiles = {
    user = user;
    email = "babathriviere@gmail.com";
  };

  users.users."${user}" = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" ];
    hashedPassword = lib.removeSuffix "\n" (builtins.readFile ./private/babariviere.passwd);
  };
}
