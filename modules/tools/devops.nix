{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.tools.devops;
in {
  options.dotfiles.tools.devops.enable = lib.mkEnableOption "devops";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs.unstable; [
      terraform
      packer
      ansible
      pkgs.nixops
    ];
  };
}
