{ lib, ... }:

with lib; {
  git = mkOption {
    type = types.submodule {
      options = {
        enable = mkEnableOption "git";
        signingKey = mkOption {
          type = types.nullOr types.str;
          description = "signing key for commits";
          default = null;
        };
        extraConfig = mkOption {
          type = types.attrs;
          description = "extra config for git";
          default = { };
        };
      };
    };

    default = { };
  };
}
