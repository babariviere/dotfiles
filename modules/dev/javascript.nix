{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  options.dotfiles.dev.javascript.enable = lib.mkEnableOption "javascript";

  config = lib.mkIf cfg.dev.javascript.enable {
    environment.systemPackages = with pkgs; [
      nodejs
      nodePackages.typescript
      nodePackages.typescript-language-server
      yarn
    ];
  };
}
