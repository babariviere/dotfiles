{ config, lib, pkgs, ... }:

let
  cfg = config.services.dotfiles;
in
{
  environment = {
    systemPackages = with pkgs; [
      git
      gitAndTools.hub
      gitAndTools.diff-so-fancy
    ];

    shellAliases = {
      ga = "git add";
      gc = "git commit";
      gcm = "git commit -m";
      gp = "git push";
      gpl = "git pull --rebase --autostash";
    };
  };

  home-manager.users."${cfg.user}".programs.git = {
    enable = true;
    extraConfig = {
      core = {
        pager = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -RFX";
      };
    };
  };
}
