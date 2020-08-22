{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.editors.vscode;
in {
  options.dotfiles.editors.vscode.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ unstable.vscode ];
  };
}
