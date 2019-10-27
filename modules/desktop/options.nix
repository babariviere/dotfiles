{ lib, ... }:

with lib; {
  enable = mkEnableOption "desktop";

  ligature = mkOption {
    type = types.bool;
    description = "Add ligature support to all services";
    default = true;
  };
}
