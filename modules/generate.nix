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
        value = mkOption {
          type =
            types.submodule { options = { enable = mkEnableOption name; }; };
          # TODO: description
          default = { };
        };
      }) (filterAttrs submoduleP (builtins.readDir mpath));
in {
  # TODO: generate example
  # TODO: better merge for submodule
  # warning: this does not support path with depth > 1
  # [path] -> set
  options = with lib;
    imports:
    foldl (m: path:
      let
        optionsPath = path + /options.nix;
        extraOptions = if (builtins.pathExists optionsPath) then
          import optionsPath { inherit lib; }
        else
          { };
        name = builtins.baseNameOf path;
      in m // {
        "${name}" = mkOption {
          type = types.submodule {
            options = (optionsSubmodules path) // extraOptions;
          };
          description = name + " submodule.";
          # TODO: find a way to add description.
          default = { };
        };
      }) { } imports;
}
