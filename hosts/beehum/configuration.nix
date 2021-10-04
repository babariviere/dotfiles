{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  ## Networking

  networking.hostId = "99b75f29";
  networking.hostName = "${config.meta.specie.code}";
  networking.domain = "home";

  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = true;
  networking.useNetworkd = true;
  # networking.interfaces = {
  #   enp4s0.useDHCP = true;
  #   wlp0s20f3.useDHCP = true;
  # };

  systemd.services."systemd-networkd-wait-online".serviceConfig.ExecStart = [
    ""
    "${config.systemd.package}/lib/systemd/systemd-networkd-wait-online --any"
  ];

  systemd.network.enable = true;
  systemd.network.links."00-wlp0s20f3" = {
    enable = true;
    matchConfig.MACAddress = "80:b6:55:ef:fd:a3";
    linkConfig.Name = "wlp0s20f3";
  };

  systemd.network.networks.wlp0s20f3 = {
    name = "wlp0s20f3";
    networkConfig = {
      DHCP = "yes";
      IPv6PrivacyExtensions = "yes";
    };
  };

  networking.wireless.iwd.enable = true;
  networking.wireless.iwd.settings = {
    General = {
      UseDefaultInterface = true;
      EnableNetworkConfiguration = true;
      AddressRandomization = "once";
    };
  };

  ## Profiles

  profiles = {
    desktop.sway.enable = true;
    login.greetd.enable = true;
    media.pulseaudio.enable = true;
    net.tailscale.enable = true;
  };

  ## Display

  # TODO: configuration

  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";

  ## Nix

  ## Audio

  ## Security

  security.pam.loginLimits = [{
    domain = "*";
    type = "-";
    item = "nofile";
    value = "1048576";
  }];

  ## Wayland

  ## Virtualisation

  # Required to use podman instead of docker for building. (even if we have the alias)
  environment.systemVariables = { COMPOSE_DOCKER_CLI_BUILD = "0"; };

  virtualisation.containers = {
    enable = true;
    storage.settings = {
      storage.driver = "overlay";
      storage.options = {
        # Required with zfs, otherwise it won't work
        mount_program = "${pkgs.fuse-overlayfs}/bin/fuse-overlayfs";
      };
    };
    containersConf.settings = {
      containers.default_ulimits = [ "nofile=1048576:1048576" ];
    };
  };
  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
    dockerCompat = true;
  };
  # TODO: find a way to use podman instead
  # virtualisation.docker = {
  #   enable = true;
  #   enableOnBoot = true;
  #   autoPrune.enable = true;
  # };

  services.openvpn.servers = {
    godzilla = {
      config = ''
        client
        dev tun
        proto tcp

        resolv-retry infinite
        nobind
        persist-key
        persist-tun
        mute-replay-warnings
        cipher AES-256-CBC
        comp-lzo
        verb 4

        # Choose one
        #remote 10.208.27.158 1194
        remote 35.205.77.46 1194

        # Configure the paths to your key files
        ca /etc/openvpn/godzilla/ca.crt
        cert /etc/openvpn/godzilla/client.crt
        key /etc/openvpn/godzilla/client.key
      '';
    };
  };

  # User
  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "docker" "podman" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
    shell = pkgs.fish;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        common-lisp.enable = true;
        go.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      desktop.sway.enable = true;
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

    # TODO: remove this and split in profile
    home.packages = with pkgs; [
      firefox
      conan
      rofi
      mako
      nixos-option
      nyxt
      discord
      slack
      foot
      docker-compose
      neovim
      postgresql
      gcc
      yarn
      poetry
      font-awesome
      gitlab-ci-lint
      jq
    ];

    gtk = {
      enable = true;
      iconTheme = {
        name = "Paper-Mono-Dark";
        package = pkgs.paper-icon-theme;
      };
    };

    home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

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
          gitlab.user = "bastien.riviere";
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
