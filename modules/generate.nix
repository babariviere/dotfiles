{ lib, ... }:

let
  submoduleP = n: v:
    v == "regular" && (lib.strings.hasSuffix ".nix" n) && n != "default.nix"
    && n != "options.nix";

  optionsSubmodules = with lib;
    mpath:
    mapAttrs' (file: value:
      let name = removeSuffix ".nix" file;
      in {
        name = name;
        value = { enable = mkEnableOption name; };
      }) (filterAttrs submoduleP (builtins.readDir mpath));
in {
  # TODO: generate example
  # TODO: better merge for submodule
  # warning: this does not support path with depth > 1
  # [path] -> set
  options = with lib;
    imports:
    foldl (m: path:
      let name = builtins.baseNameOf path;
      in m // { "${name}" = (optionsSubmodules path); }) { } imports;
}
