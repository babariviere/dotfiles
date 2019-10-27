{ lib, ... }:

with lib; {
  git = mkOption {
    type = types.submodule {
      options = {
        enable = mkEnableOption "git";
        signingKey = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
      };
    };

    default = { };
  };
}
