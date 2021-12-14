{ config, lib, pkgs, ... }:

{
  options.shell = with lib; {
    env = mkOption {
      default = { };
      type = types.attrs;
      description = ''
        Environment variables that will be set for all shells.
      '';
    };
    aliases = mkOption {
      default = { };
      type = types.attrsOf types.str;
      description = ''
        Add aliases (or abbreviations for fish) for all shells.
      '';
    };
  };

  config = let sessionVarsStr = config.lib.shell.exportAll config.shell.env;
  in lib.mkMerge [
    {
      programs.bash = {
        sessionVariables = config.shell.env;
        shellAliases = config.shell.aliases;
      };
      programs.fish = {
        shellInit = lib.concatStringsSep "\n"
          (lib.mapAttrsToList (n: v: ''set -g ${n} "${v}"'') config.shell.env);
        shellAbbrs = config.shell.aliases;
      };
      programs.zsh = {
        shellAliases = config.shell.aliases;
        envExtra = sessionVarsStr;
      };
    }
    (lib.mkIf (!config.programs.bash.enable) {
      home.file.".profile".source = pkgs.writeShellScript "profile" ''
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        ${sessionVarsStr}
        GUIX_PROFILE="$HOME/.guix-profile"
        [ -f "$GUIX_PROFILE/etc/profile" ] && . "$GUIX_PROFILE/etc/profile"

        _GUIX_PROFILE="$HOME/.config/guix/current"
        export PATH="$_GUIX_PROFILE/bin''${PATH:+:}$PATH"
      '';
    })
  ];
}
