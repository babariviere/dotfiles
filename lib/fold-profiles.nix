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
        in if t == "regular" then
          { config, lib, pkgs, ... }@attrs:
          let
            # Import the profile file by concat all paths
            content =
              import (lib.foldl (a: b: a + "/${b}") profile-dir path') attrs;
            options = content.options or { };
            content' = content.config or content;
          in {
            options.profiles = lib.setAttrByPath attrPath ({
              enable = lib.mkEnableOption
                (lib.concatStringsSep " " (attrPath ++ [ name ]));
            } // options);

            config = lib.mkIf
              (lib.getAttrFromPath ([ "profiles" ] ++ attrPath ++ [ "enable" ])
                config) content';
          }
        else
          foldDir profile-dir path' (dir + "/${n}");
    in lib.mapAttrsToList (n: v: f n v) paths;
in profile-dir: lib.flatten (foldDir profile-dir [ ] profile-dir)
