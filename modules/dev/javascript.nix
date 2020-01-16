{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.javascript.enable {
    environment.systemPackages = with pkgs; [
      nodejs
      nodePackages.typescript
      nodePackages.typescript-language-server
      yarn
    ];
  };
}
