{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.ocaml;
in {
  options.dotfiles.dev.ocaml.enable = lib.mkEnableOption "ocaml";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ ocaml ocamlformat opam ]
      ++ (with ocamlPackages; [ utop dune ocp-indent merlin reason ]);
  };
}
