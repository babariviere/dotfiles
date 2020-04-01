{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell.starship;
in {
  options.dotfiles.shell.starship.enable = lib.mkEnableOption "starship";

  config = lib.mkIf cfg.enable {
    home-manager.users."${dotfiles.user}".programs.starship = {
      enable = true;
      package = pkgs.unstable.starship;
      settings = {
        character.symbol = "➜";

        aws.symbol = " ";

        battery = {
          full_symbol = "";
          charging_symbol = "";
          discharging_symbol = "";
        };

        conda.symbol = " ";

        elixir.symbol = " ";

        git_branch.symbol = " ";

        golang.symbol = " ";

        hg_branch.symbol = " ";

        java.symbol = " ";

        memory_usage.symbol = " ";

        nodejs.symbol = " ";

        package.symbol = " ";

        php.symbol = " ";

        python.symbol = " ";

        ruby.symbol = " ";

        directory.style = "yellow";
        hostname = {
          style = "blue";
          ssh_only = false;
        };
        username = {
          style_user = "blue";
          show_always = true;
        };
      };
    };
  };
}
