{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.editors.neovim;
in {
  options.dotfiles.editors.neovim.enable = lib.mkEnableOption "neovim";

  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ neovim ]; };
}
