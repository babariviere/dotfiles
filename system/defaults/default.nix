{ config, inputs, lib, pkgs, self, system, ... }:

{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  environment.pathsToLink = [ "/share/fish" "/share/zsh" ];

  nix = {
    settings = {
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://babariviere.cachix.org"
        "https://cache.iog.io"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "babariviere.cachix.org-1:igoOZJyEAhWg3Rbavjim3yyDj7nIkGYe5327+G0diFw="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      sandbox = "relaxed";
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
      pure-eval = false
    '';
    gc = {
      automatic =
        false; # FIXME: enable only on local machines, on server it's useful
      options = "-d --delete-older-than 7d";
    };
    package = pkgs.hiPrio pkgs.nixFlakes;
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      dotfiles.flake = self;
    };
  };

  environment.systemPackages = with pkgs;
    [ git emacs wget curl ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs = {
    overlays = lib.attrValues self.overlays;
    config.allowUnfree = true;
  };

  system.configurationRevision = with inputs; lib.mkIf (self ? rev) self.rev;
  system.stateVersion = "22.05";
}
