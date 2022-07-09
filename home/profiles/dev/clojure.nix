{ config, lib, pkgs, ... }:

{
  home.packages =
    [ pkgs.babashka pkgs.boot pkgs.clojure pkgs.clojure-lsp pkgs.leiningen ];
}
