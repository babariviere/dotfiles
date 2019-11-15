{ lib, ... }:

let
  submoduleP = path: n: v:
    if v == "directory" then
      (builtins.readDir (path + "/${n}")) ? "default.nix"
    else
      v == "regular" && (lib.strings.hasSuffix ".nix" n) && n != "default.nix"
      && n != "options.nix";

  filterSubmodules = path:
    let files = builtins.readDir path;
    in lib.filterAttrs (submoduleP path) files;

  optionsSubmodules = with lib;
    mpath:
    mapAttrs' (file: value:
      let name = removeSuffix ".nix" file;
      in {
        name = name;
        value = { enable = mkEnableOption name; };
      }) (filterSubmodules mpath);
in {
  # TODO: generate example
  # [path] -> set
  options = with lib;
    imports:
    foldl (m: path:
      let name = builtins.baseNameOf path;
      in m // { "${name}" = (optionsSubmodules path); }) { } imports;
}
