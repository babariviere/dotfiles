{ config, lib, pkgs, ... }:

with lib;
{
  options.services.dotfiles = {
    user = mkOption {
      type = types.string;
      description = "User used for services";
    };

    ligature = mkOption {
      type = types.bool;
      description = "Add ligature support to all services";
      default = true;
    };
  };
}
