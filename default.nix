{ config, lib, pkgs, ... }:

# Default configuration for dotfiles
{
  imports = [ ./themes ./modules ];

  dotfiles.name = "Bastien Rivière";

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users."${config.dotfiles.user}".home.sessionVariables =
    import ./env.nix { inherit lib; };
}
