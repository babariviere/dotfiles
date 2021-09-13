{ lib }:

# foldProfiles : path -> [path]
let
  removeSuffix = suffix: str: builtins.head (builtins.split suffix str);
  # foldDir: path -> [string] -> path -> [ lambda ]
  #
  # profile-dir: root path of the profile dir, used to trim path
  # path: path list to get to the current file, e.g. [ "shell" ]
  # dir: directory to read, e.g. ./profiles/shell
  foldDir = profile-dir: path: dir:
    let
      paths = builtins.readDir dir;
      f = n: t:
        let
          name = removeSuffix ".nix" n;
          attrPath = if n != "default.nix" then (path ++ [ name ]) else path;
          path' = path ++ [ n ];
          isHidden = lib.hasPrefix "." name;
        in if t == "regular" && !isHidden then
          { config, lib, ... }@attrs:
          let
            # Import the profile file by concat all paths
            f = import (lib.foldl (a: b: a + "/${b}") profile-dir path');
            args = (builtins.intersectAttrs (builtins.functionArgs f) attrs);
            content = f args;
            options = if content ? options then content.options else { };
            content' = if content ? config then content.config else content;
          in {
            options.profiles = lib.setAttrByPath attrPath ({
              enable = lib.mkEnableOption
                (lib.concatStringsSep " " (attrPath ++ [ name ]));
            } // options);

            config = lib.mkIf
              (lib.getAttrFromPath ([ "profiles" ] ++ attrPath ++ [ "enable" ])
                config) content';
          }
        else if t == "directory" && !isHidden then
          foldDir profile-dir path' (dir + "/${n}")
        else
          [ ];
    in lib.mapAttrsToList (n: v: f n v) paths;
in profile-dir: lib.flatten (foldDir profile-dir [ ] profile-dir)
