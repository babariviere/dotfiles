{ config, lib, ... }:

with lib; {
  options = with types; {
    dotfiles = let type = path;
    in {
      dir = mkOption {
        inherit type;
        default = findFirst pathExists (toString ../../.) [
          "${config.home.homeDirectory}/src/github.com/babariviere/dotfiles"
          "${config.home.homeDirectory}/src/github.com/babariviere/dot-v6"
          "${config.home.homeDirectory}/src/github.com/babariviere/dot"
        ];
        readOnly = true;
      };
      configDir = mkOption {
        inherit type;
        default = "${config.dotfiles.dir}/etc";
        readOnly = true;
      };
    };
  };
}
