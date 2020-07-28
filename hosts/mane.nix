{ config, lib, pkgs, priv, ... }:

let user = "babariviere";
in {
  imports = [
    ../.
    ../hardware/mane.nix
    ../profiles/games
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
    theme.name = "one";
    network = {
      eth = "enp0s31f6";
      wlan = "wlp82s0";
    };
    desktop = {
      enable = true;
      awesome.enable = false;
      bspwm.enable = false;
      chrome.enable = true;
      xmonad.enable = true;
      #i3.enable = true;
      firefox.enable = true;
      polybar.enable = false;
      alacritty.enable = true;
      picom.enable = false;
      dunst.enable = true;
      rofi.enable = true;
      thunderbird.enable = false;
    };
    dev = {
      android = {
        enable = true;
        studio = true;
      };
      c.enable = true;
      elixir.enable = true;
      elm.enable = false;
      godot.enable = false;
      guile.enable = false;
      haskell.enable = true;
      idris.enable = true;
      javascript.enable = true;
      latex.enable = true;
      lua.enable = true;
      ocaml.enable = false;
      plantuml.enable = true;
      python.enable = true;
      ruby.enable = true;
      rust.enable = true;
      web.enable = true;
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
      fish.enable = false;
      git = {
        enable = true;
        signingKey = "D168892C2E1F486A011F205CB6CA57D2225BB9CE";
      };
      starship.enable = true;
      zsh.enable = true;
    };
    services = {
      bitwarden.enable = true;
      fingerprint.enable = true;
      fwupd.enable = true;
      gpg.enable = true;
      guix.enable = false;
      keyring.enable = true;
      mail.enable = true;
      postgres.enable = true;
      ssh.enable = true;
      syncthing.enable = false;
      libvirtd.enable = true;
      virtualbox.enable = false;
      zerotier = {
        enable = true;
        networks = import (priv "zt-networks.nix");
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
      docker = {
        enable = true;
        compose = true;
      };
      light.enable = true;
      podman = {
        enable = false;
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

  environment.systemPackages =
    # flutter = (import (builtins.fetchTarball
    #   "https://github.com/babariviere/nixpkgs/archive/flutter-init.tar.gz")
    #   { }).flutterPackages.beta; # TODO: remove me when official
    with pkgs;
    [
      # unstable.next # TODO: this makes me compile qt-webengine
      pkgs.unstable.bandwhich
    ];
}
