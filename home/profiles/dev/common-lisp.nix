{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.sbcl pkgs.clpm ];

  home.file.".sbclrc".source = pkgs.runCommandLocal "sbclrc" {
    source = builtins.readFile (config.dotfiles.configDir + "/sbclrc");
    passAsFile = [ "source" ];
  } ''
    ${pkgs.clpm}/bin/clpm client rc > $out
    cat $sourcePath >> $out
  '';

  xdg.configFile."clpm/sources.conf".text = ''
    ("quicklisp"
     :type :quicklisp
     :url "https://beta.quicklisp.org/dist/quicklisp.txt")
  '';
  xdg.configFile."common-lisp/source-registry.conf.d/20-clpm-client.conf".source =
    pkgs.runCommandLocal "clpm-client.conf" { } ''
      ${pkgs.clpm}/bin/clpm client source-registry.d > $out
    '';

  xdg.configFile."common-lisp/source-registry.conf.d/50-projects.conf".text = ''
    (:directory (:home "src/github.com/babariviere/yas"))
  '';
}
