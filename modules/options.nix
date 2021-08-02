{ config, options, lib, home-manager, pkgs, ... }:

with lib; {
  options = with types; {
    dotfiles = let
      type = path;
      forEachUser = f:
        map f (filter (user: user.isNormalUser or true)
          (attrValues config.users.users));
    in {
      dir = mkOption {
        inherit type;
        default = toString ../.;
        readOnly = true;
      };
      configDir = mkOption {
        inherit type;
        default = "${config.dotfiles.dir}/config";
        readOnly = true;
      };
    };

    meta.specie = mkOption {
      type = attrs;
      default = { };
    };
  };
}
