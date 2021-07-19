{ config, options, lib, home-manager, pkgs, ... }:

with lib; {
  options = with types; {
    dotfiles = let type = path;
    in {
      dir = mkOption {
        inherit type;
        default = /. + findFirst pathExists (toString ../.) [
          "${config.user.home}/src/github.com/babariviere/dotfiles"
          "${config.user.home}/.config/dotfiles"
          "/etc/dotfiles"
        ];
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

  # TODO(babariviere): find a way to make this optional
  imports = [
    (mkAliasOptionModule [ "hm" ] [ "home-manager" "users" config.user.name ])
    (mkAliasOptionModule [ "home" ] [ "hm" "home" ])
    (mkAliasOptionModule [ "env" ] [ "hm" "home" "sessionVariables" ])
  ];

  config = {
    user = {
      home = if pkgs.stdenv.isDarwin then
        "/Users/${config.user.name}"
      else
        "/home/${config.user.name}";
    } // (lib.optionalAttrs pkgs.stdenv.isLinux {
      extraGroups = [ "wheel" ];
      isNormalUser = true;
    });

    users.users.${config.user.name} = mkAliasDefinitions options.user;
  };
}
