{ config, lib, pkgs, ... }:

{
  options.env = lib.mkOption {
    default = { };
    type = lib.types.attrs;
    description = ''
      Environment variables that will be set for all shells.
    '';
  };

  config = let sessionVarsStr = config.lib.shell.exportAll config.env;
  in lib.mkMerge [
    {
      programs.bash.sessionVariables = config.env;
      programs.zsh.sessionVariables = config.env;
    }
    (lib.mkIf (!config.programs.bash.enable) {
      home.file.".profile".source = pkgs.writeShellScript "profile" ''
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        ${sessionVarsStr}
      '';
    })
  ];
}
