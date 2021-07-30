{ lib }:

let
  removeSuffix = suffix: str: builtins.head (builtins.split suffix str);
  foldDir = profile-dir: path: dir:
    let
      paths = builtins.readDir dir;
      f = n: t:
        let
          name = removeSuffix ".nix" n;
          attrPath = (path ++ [ name ]);
          path' = path ++ [ n ];
        in if t == "regular" then
          { config, ... }@attrs: {
            options.profiles = lib.setAttrByPath attrPath {
              enable = lib.mkEnableOption
                (lib.concatStringsSep " " (attrPath ++ [ name ]));
            };

            config = lib.mkIf
              (lib.getAttrFromPath ([ "profiles" ] ++ attrPath ++ [ "enable" ])
                config)
              (import (lib.foldl (a: b: a + "/${b}") profile-dir path') attrs);
          }
        else
          foldDir profile-dir path' (dir + "/${n}");
    in lib.mapAttrs' (n: v: lib.nameValuePair (removeSuffix ".nix" n) (f n v))
    paths;
in profile-dir: foldDir profile-dir [ ] profile-dir
