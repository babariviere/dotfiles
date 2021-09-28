{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  ## Networking

  networking.hostId = "99b75f29";
  networking.hostName = "${config.meta.specie.code}";
  networking.domain = "home";

  networking.useDHCP = false;
  networking.interfaces = {
    enp4s0.useDHCP = true;
    wlp0s20f3.useDHCP = true;
  };

  networking.wireless.iwd.enable = true;

  ## Profiles

  profiles = { net.tailscale.enable = true; };

  ## Display

  # TODO: configuration
  programs.sway.enable = true;
  services.greetd.enable = true;
  services.greetd.settings.default_session.command =
    "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd sway";

  # FIXME: what do I need to do this?
  users.users.greeter.group = "greeter";
  users.groups.greeter = { };

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";

  # User

  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
    shell = pkgs.fish;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        go.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      editor = {
        emacs.enable = true;
        editorconfig.enable = true;
      };
      shell = {
        common.enable = true;
        direnv = {
          enable = true;
          nix = true;
        };
        fish.enable = true;
        git.enable = true;
        gh.enable = true;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    # TODO: refactor me
    programs.git = {
      extraConfig = {
        user.signingKey = "F9B7864F2AB46F18";
        commit.gpgSign = true;
      };
      includes = [{
        condition = "gitdir:~/src/gitlab.com/TankerHQ/";
        contents = {
          user = {
            email = "bastien.riviere@tanker.io";
            signingKey = "ACFD416C8BFB251A";
          };
          commit.gpgSign = true;
        };
      }];
    };

    programs.gpg.enable = true;

    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };

    home.stateVersion = "21.03";
  };

  # Meta

  system.stateVersion = "21.11";

  meta = {
    specie = {
      url = "https://ebird.org/species/beehum1";
      code = "beehum";
      name = "Bee Hummingbird";
    };
  };
}
