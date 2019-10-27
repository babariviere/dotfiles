{ config, lib, pkgs, ... }:

let cfg = config.services.dotfiles.editors.neovim;
in {
  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ neovim ]; };
}
