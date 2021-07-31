{ config, lib, ... }:

with lib; {
  options = with types; {
    dotfiles = let type = either str path;
    in {
      dir = mkOption {
        inherit type;
        default = findFirst pathExists (toString ../../.) [
          "${config.home.homeDirectory}/src/github.com/babariviere/dotfiles"
          "${config.home.homeDirectory}/.config/dotfiles"
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
  };
}
