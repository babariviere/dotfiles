{ lib, ... }:

with lib; {
  emacs = mkOption {
    type = types.submodule {
      options = {
        enable = mkEnableOption "emacs";
        editorconfig = mkOption {
          type = types.bool;
          description = "Add support for editorconfig";
          default = true;
        };
        ripgrep = mkOption {
          type = types.bool;
          description = "Add support for ripgrep";
          default = true;
        };
      };
    };
    default = { };
  };
}
