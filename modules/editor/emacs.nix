{ config, lib, pkgs, ... }:

let cfg = config.my.editor.emacs;
    emacs = pkgs.emacsOsx;
    in
{
  options.my.editor.emacs = with lib; {
    enable = mkEnableOption "emacs";
  };

  config = lib.mkIf cfg.enable {
  services.emacs = {
    enable = true;
    # TODO: Find a way to shorten this path
    package = emacs;
  };
  };
}
