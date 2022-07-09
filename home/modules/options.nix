{ config, lib, ... }:

with lib; {
  options = with types; {
    dotfiles = let type = either str path;
    in {
      dir = mkOption {
        inherit type;
        default = toString ../../.;
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
