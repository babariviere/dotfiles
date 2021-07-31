{ config, options, lib, home-manager, pkgs, ... }:

with lib; {
  options = with types; {
    dotfiles = let
      type = either str path;
      forEachUser = f: map f (attrValues config.users.users);
    in {
      dir = mkOption {
        inherit type;
        default = let
          paths = (forEachUser
            (user: "${user.home}/src/github.com/babariviere/dotfiles"))
            ++ (forEachUser (user: "${user.home}/.config/dotfiles"))
            ++ [ "/etc/dotfiles" ];
        in findFirst pathExists (toString ../.) paths;
      };
      configDir = mkOption {
        inherit type;
        default = "${config.dotfiles.dir}/config";
      };
      modulesDir = mkOption {
        inherit type;
        default = "${config.dotfiles.dir}/modules";
      };
    };

    user = mkOption {
      type = attrs;
      default = { };
    };

    meta.specie = mkOption {
      type = attrs;
      default = { };
    };
  };
}
