{ config, lib, pkgs, ... }:

let user = "babariviere";
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
    email = "dev@babariviere.com";
    theme = "dracula";
    network = {
      eth = "enp0s31f6";
      wlan = "wlp82s0";
    };
    desktop = {
      enable = true;
      bspwm.enable = true;
      #i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
      compton.enable = false;
      dunst.enable = true;
      rofi.enable = true;
    };
    dev = {
      android.enable = true;
      haskell.enable = true;
      rust.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    media = {
      plex.enable = true;
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
      ssh.enable = true;
      syncthing.enable = true;
    };
    social = {
      discord.enable = true;
      riot.enable = true;
    };
    tools = {
      build.enable = true;
      devops.enable = true;
      light.enable = true;
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

  ## Custom settings
  services.undervolt = {
    enable = true;
    coreOffset = "-100";
  };

  services.xserver.libinput = { accelSpeed = "0.4"; };
  services.zfs.autoScrub.enable = true;
}
