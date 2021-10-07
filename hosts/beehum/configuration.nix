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
  # TODO: use network.nix
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "100.100.28.13";
    maxJobs = 8;
    sshUser = "root";
    system = "x86_64-linux";
    sshKey = "/root/.ssh/id_ed25519";
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  }];

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
  # environment.sessionVariables = { COMPOSE_DOCKER_CLI_BUILD = "0"; };

  # virtualisation.containers = {
  #   enable = true;
  #   storage.settings = {
  #     storage.driver = "overlay";
  #     storage.options = {
  #       # Required with zfs, otherwise it won't work
  #       mount_program = "${pkgs.fuse-overlayfs}/bin/fuse-overlayfs";
  #     };
  #   };
  #   containersConf.settings = {
  #     containers.default_ulimits = [ "nofile=1048576:1048576" ];
  #   };
  # };
  # # FIXME: random crash with postgres
  # virtualisation.podman = {
  #   enable = true;
  #   dockerSocket.enable = true;
  #   dockerCompat = true;
  # };
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
  };

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
      ccls
      clang-tools
      yaml-language-server
      xdg_utils
    ];

    gtk = {
      enable = true;
      gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
      font = {
        package = pkgs.roboto;
        name = "Roboto";
        size = 10;
      };
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };
      theme = {
        name = "Orchis-dark";
        package = pkgs.orchis-theme;
      };
    };

    xsession.pointerCursor = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
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
          core.excludesFile = let
            file = pkgs.writeText "gitignore" ''
              shell.nix
              .envrc
              .direnv
              .go
              flake.nix
              flake.lock
              arion-compose.nix
              arion-pkgs.nix
            '';
          in "${file}";
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
      defaultCacheTtl = 1800;
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
