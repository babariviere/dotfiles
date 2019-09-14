{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (lib.mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)

    # Essential
    emacs
    editorconfig-core-c            # :tools editorconfig

    # Misc
    direnv                         # :tools direnv
    texlive.combined.scheme-medium # :lang org
  ];
}
