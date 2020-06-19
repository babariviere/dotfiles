{ config, lib, pkgs, priv, ... }:

let user = "bastien";
in {
  imports = [
    ../.
    ../hardware/sollys.nix
    ../private/sollys/extra.nix
    ../profiles/nvidia.nix
  ];

  dotfiles = {
    user = user;
    email = "bastien.riviere@easy.movie";
    theme.name = "one";
    network = {
      eth = "enp59s0";
      wlan = "wlo1";
    };
    desktop = {
      enable = true;
      bspwm.enable = false;
      xmonad.enable = true;
      # i3.enable = true;
      firefox.enable = true;
      polybar = {
        enable = false;
        battery = "BAT1";
        batteryAdapter = "ADP1";
      };
      alacritty.enable = true;
      compton.enable = false;
      dunst.enable = true;
      rofi.enable = true;
    };
    dev = {
      android.enable = true;
      c.enable = true;
      go.enable = true;
      javascript.enable = true;
      php.enable = true;
      plantuml.enable = true;
      python.enable = true;
      rust.enable = true;
      web.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    shell = {
      direnv.enable = true;
      fish.enable = true;
      git = {
        enable = true;
        # signingKey = "C45AE603B0DB35266E0E1BBA7014714FCF05D20E";
      };
      starship.enable = true;
      zsh.enable = true;
    };
    services = {
      gpg.enable = true;
      keyring.enable = true;
      ssh.enable = true;
    };
    social = { slack.enable = true; };
    tools = {
      aws.enable = true;
      build.enable = true;
      docker = {
        enable = true;
        arion = true;
        compose = true;
      };
      devops.enable = true;
      insomnia.enable = true;
      light.enable = true;
      podman = {
        enable = true;
        arion = true;
        compose = true;
      };
      sql.enable = true;
    };
  };

  users.users."${user}" = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" ];
    hashedPassword =
      lib.removeSuffix "\n" (builtins.readFile (priv "${user}.passwd"));
  };

  networking.hostName = "sollys";
  environment.variables = { HOSTNAME = "sollys"; };

  ## Display settings
  services.xserver.xrandrHeads = [
    {
      output = "eDP-1-1";
      primary = true;
    }
    {
      output = "HDMI-0";
      monitorConfig = ''
        Option "Above" "eDP-1-1"
      '';
    }
  ];

  # CPU
  powerManagement.cpufreq.max = 2500000;
}
