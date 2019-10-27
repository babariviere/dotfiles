{ config, lib, pkgs, ... }:

let
  dotfiles = config.services.dotfiles;
  cfg = dotfiles.shell.git;
in {
  config = lib.mkIf cfg.enable {
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

    home-manager.users."${dotfiles.user}".programs.git = {
      enable = true;

      extraConfig = {
        core = {
          pager =
            "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -RFX";
        };
        # Color defined by diff-so-fancy
        color = {
          ui = true;
          diff-highlight = {
            oldNormal = "red bold";
            oldHighlight = "red bold 52";
            newNormal = "green bold";
            newHighlight = "green bold 52";
          };
          diff = {
            meta = "11";
            frag = "magenta bold";
            commit = "yellow bold";
            old = "red bold";
            new = "green bold";
            whitespace = "red reverse";
          };
        };
        url = {
          "git@bitbucket.org:" = { insteadOf = "https://bitbucket.org"; };
        };
      };
      signing = lib.mkIf (cfg.signingKey != null) {
        key = cfg.signingKey;
        signByDefault = true;
      };
      userEmail = dotfiles.email;
      userName = dotfiles.name;
    };
  };
}
