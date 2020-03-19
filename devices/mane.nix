{ config, lib, pkgs, ... }:

let
  user = "babariviere";
  unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
in {
  imports = [
    ../.
    ../profiles/intel.nix
    ../profiles/laptop.nix
    ../profiles/nvidia.nix
    ../profiles/ssd.nix
    ../profiles/thinkpad.nix
  ];

  dotfiles = {
    user = user;
    # email = "me@babariviere.com"; TODO: need to fix signing key
    email = "babathriviere@gmail.com";
    theme = "one";
    network = {
      eth = "enp0s31f6";
      wlan = "wlp82s0";
    };
    desktop = {
      enable = true;
      bspwm.enable = true;
      chrome.enable = true;
      #i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
      compton.enable = false;
      dunst.enable = true;
      rofi.enable = true;
      thunderbird.enable = true;
    };
    dev = {
      android = {
        enable = true;
        studio = true;
      };
      elixir.enable = true;
      godot.enable = false;
      haskell.enable = false;
      javascript.enable = true;
      latex.enable = true;
      plantuml.enable = true;
      python.enable = true;
      rust.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    media = {
      plex.enable = false;
      spotify.enable = true;
    };
    shell = {
      direnv.enable = true;
      git = {
        enable = true;
        signingKey = "6BCFEDF322EB0040B5FC4296EECF965F5AAA4E1A";
      };
      zsh.enable = true;
    };
    services = {
      bitwarden.enable = true;
      fwupd.enable = true;
      gpg.enable = true;
      keyring.enable = true;
      mail.enable = false;
      postgres.enable = true;
      ssh.enable = true;
      syncthing.enable = true;
      virtualbox.enable = false;
      zerotier = {
        enable = true;
        networks = import ../private/zt-networks.nix;
        nodes = [
          {
            name = "rpi4";
            ip = "10.0.222.186";
          }
          {
            name = "sky";
            ip = "10.0.222.212";
          }
        ];
      };
    };
    social = {
      discord.enable = true;
      riot.enable = true;
      signal.enable = true;
    };
    tools = {
      build.enable = true;
      devops.enable = true;
      docker.enable = true;
      light.enable = true;
      sql.enable = true;
    };
  };

  users.users."${user}" = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" ];
    hashedPassword =
      lib.removeSuffix "\n" (builtins.readFile ../private/babariviere.passwd);
  };

  networking.hostName = "mane";
  environment.variables = { HOSTNAME = "mane"; };

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

  ## Custom settings
  services.undervolt = {
    enable = true;
    coreOffset = "-100";
  };

  services.xserver.libinput = { accelSpeed = "0.4"; };
  services.zfs.autoScrub.enable = true;

  # enable emulation of certains system
  # boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  nix.buildMachines = [{
    hostName = "rpi4.zt";
    system = "aarch64-linux";
    maxJobs = 4;
    speedFactor = 4;
  }
  # {
  #   hostName = "sky.zt"; TODO: disable upload
  #   system = "x86_64-linux";
  #   maxJobs = 2;
  #   speedFactor = 2;
  # }
    ];
  nix.distributedBuilds = true;

  environment.systemPackages = let
    flutter = (import (builtins.fetchTarball
      "https://github.com/babariviere/nixpkgs/archive/flutter-init.tar.gz")
      { }).flutterPackages.beta; # TODO: remove me when official
  in with pkgs; [ # TODO: clean me
    flutter
    # unstable.next # TODO: this makes me compile qt-webengine
    unstable.bandwhich
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
}
