{config, options, lib, home-manager, ...}:

with lib;
{
  options = with types; {
    hm = mkOption {
      type = attrs;
      default = {};
    };

    user = mkOption {
      type = attrs;
      default = {};
    };
  };

  home-manager.users.${config.user.name} = mkAliasDefinitions options.hm;

  users.users.${config.user.name} = mkAliasDefinitions options.user;
}
