{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.sbcl ];

  home.file.".sbclrc".source = config.dotfiles.configDir + "/sbclrc";

  xdg.configFile."common-lisp/source-registry.conf.d/50-projects.conf".text = ''
    (:tree (:home "src/github.com/babariviere"))
  '';
}
