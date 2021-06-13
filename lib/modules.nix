{ lib, ... }:

let
  inherit (builtins) readDir pathExists toPath;
  inherit (lib) flatten hasSuffix mapAttrsToList;
in rec {
  # Find modules in directory (not recursive)
  #
  # findModules :: path -> list(path)
  findModules = dir:
    mapAttrsToList (name: type:
      let path = "${toString dir}/${name}";
      in if type == "directory"
      && builtins.pathExists "${path}/default.nix" then
        path
      else if type == "regular" && name != "default.nix"
      && hasSuffix ".nix" name then
        path
      else
        null) (readDir dir);

  # Find modules in directory recursively.
  #
  # findModulesRec :: path -> list(path)
  findModulesRec = dir:
    let
      modules = mapAttrsToList (name: type:
        let path = "${toString dir}/${name}";
        in if type == "directory" then
          (findModulesRec path)
        else if name == "default.nix" then
          dir
        else if hasSuffix ".nix" name then
          path
        else
          null) (readDir dir);
    in flatten modules;
}
