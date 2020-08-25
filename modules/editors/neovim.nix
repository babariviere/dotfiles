{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.editors.neovim;
in {
  options.dotfiles.editors.neovim.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf cfg.enable {
    # TODO: install ranger as another module
    environment.systemPackages = with pkgs; [
      neovim
      xsel
      ranger
      ueberzug
      fzf
      silver-searcher
      ripgrep
      fd
    ];
  };
}
