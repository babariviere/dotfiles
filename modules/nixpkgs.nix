{ config, lib, options, pkgs, ... }:

let
  sources = import ../nix/sources.nix;
  cfg = config.dotfiles;
in {
  nixpkgs = {
    pkgs = import sources.nixpkgs { config = { allowUnfree = true; }; };
    overlays = import ../overlays.nix;
  };

  environment = {
    etc.nixpkgs.source = sources.nixpkgs;
    systemPackages = [ (import sources.niv { }).niv pkgs.nixFlakes ];
  };

  nix = {
    useSandbox = true;
    gc = {
      automatic = true;
      dates = "*-*-* 18:00:00";
      options = "--delete-older-than 7d";
    };
    nixPath = options.nix.nixPath.default ++ [
      "config=/etc/dotfiles/config"
      "nixpkgs-overlays=/etc/dotfiles/overlays.nix"
    ];
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" cfg.user ];
    binaryCaches = [
      "https://aseipp-nix-cache.freetls.fastly.net"
      "https://babariviere.cachix.org"
      "https://all-hies.cachix.org"
      "https://cache.nixos.org"
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "babariviere.cachix.org-1:IZH469NpgfLwJWYS2qv6HsuaKpBP7AwEqfkpcfd/U04="
    ];
    extraOptions = ''
      # Set TTL to one day
      tarball-ttl = 86400
      experimental-features = nix-command flakes ca-references
    '';
  };
  # run gc only if power source is plugged
  systemd.services.nix-gc.unitConfig.ConditionACPower = true;

  # force the use of niv's nixpkgs for system packages
  system.activationScripts.update-channel = ''
    mkdir -p /nix/var/nix/profiles/per-user/root/channels-system
    ln -sfn ${sources.nixpkgs} /nix/var/nix/profiles/per-user/root/channels-system/nixos
    ln -sfn /nix/var/nix/profiles/per-user/root/channels-system /nix/var/nix/profiles/per-user/root/channels
  '';

  # TODO: auto git clone

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
  system.autoUpgrade = {
    enable = true;
    channel = sources.nixpkgs.outPath;
  };
  systemd.services.nixos-upgrade.unitConfig.ConditionACPower = true;
  systemd.timers.nixos-upgrade.timerConfig.Persistent = true;
}
