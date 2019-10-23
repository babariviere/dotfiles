{ config, lib, pkgs, ... }:

let
  user = "bastien";
in
{
  imports = [
    ./.

    ./modules/desktop
    ./modules/desktop/i3.nix
    ./modules/dev/go.nix
    ./modules/editors/emacs.nix
    ./modules/shell/direnv.nix
    ./modules/shell/git.nix
    ./modules/shell/zsh.nix
    ./modules/social/slack.nix
    ./modules/tools/build.nix
    ./modules/tools/devops.nix
  ];

  services.dotfiles = {
    user = user;
    email = "bastien.riviere@easy.movie";
  };

  users.users."${user}" = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" ];
    hashedPassword = lib.removeSuffix "\n" (builtins.readFile ./private/bastien.passwd);
  };
}
