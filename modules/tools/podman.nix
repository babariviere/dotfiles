{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.tools.podman;
  sources = import ../../nix/sources.nix;
  unstable = import sources.unstable {
    config = removeAttrs config.nixpkgs.config [ "packageOverrides" ];
  };
in {
  imports = [
    "${sources.unstable}/nixos/modules/virtualisation/containers.nix"
    "${sources.unstable}/nixos/modules/virtualisation/podman.nix"
    "${sources.unstable}/nixos/modules/virtualisation/oci-containers.nix"
  ];

  disabledModules = [ "virtualisation/docker-containers.nix" ];

  options.dotfiles.tools.podman = {
    enable = lib.mkEnableOption "podman";
    arion = lib.mkEnableOption "arion";
    compose = lib.mkEnableOption "podman-compose";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      enable = true;
      dockerCompat = !dotfiles.tools.docker.enable;
      package = pkgs.unstable.podman;
    };

    dotfiles.tools.virtualisation.enable = true;
    virtualisation.oci-containers.backend = "podman";

    environment.systemPackages = with pkgs; [
      (lib.mkIf cfg.compose podman-compose)
      (lib.mkIf cfg.arion arion)
    ];

    users.users."${dotfiles.user}".extraGroups = [ "podman" ];
  };
}
