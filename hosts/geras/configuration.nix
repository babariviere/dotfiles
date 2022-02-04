{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.blacklistedKernelModules = [ "nouveau" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  ## Networking

  networking.hostId = "99b75f29";
  networking.hostName = "geras";
  networking.domain = "home";

  networking.nameservers = [ "1.1.1.1" ];
  networking.wireless = {
    enable = true;
    interfaces = [ "wlp0s20f3" ];
    allowAuxiliaryImperativeNetworks = true;
  };
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = true;
  # networking.useNetworkd = true;
  networking.interfaces = {
    enp4s0.useDHCP = false;
    wlp0s20f3.useDHCP = true;
  };

  services.yggdrasil = {
    enable = false;
    config = {
      Peers = [
        "tls://fr2.servers.devices.cwinfo.net:23108"
        "tls://cloudberry.fr1.servers.devices.cwinfo.net:54232"
      ];
      Listen = [ "tcp://0.0.0.0:0" ];
    };
  };

  programs.ccache.enable = true;

  # systemd.services."systemd-networkd-wait-online".serviceConfig.ExecStart = [
  #   ""
  #   "${config.systemd.package}/lib/systemd/systemd-networkd-wait-online --any"
  # ];

  # systemd.network.enable = true;
  # systemd.network.links."00-wlp0s20f3" = {
  #   enable = true;
  #   matchConfig.MACAddress = "80:b6:55:ef:fd:a3";
  #   linkConfig.Name = "wlp0s20f3";
  # };

  # systemd.network.networks.wlp0s20f3 = {
  #   name = "wlp0s20f3";
  #   networkConfig = {
  #     DHCP = "yes";
  #     IPv6PrivacyExtensions = "yes";
  #     DNS = [ "1.1.1.1" ];
  #   };
  # };

  # networking.wireless.iwd.enable = true;
  # networking.wireless.iwd.settings = {
  #   General = {
  #     UseDefaultInterface = true;
  #     EnableNetworkConfiguration = true;
  #     AddressRandomization = "once";
  #   };
  #   Network = { NameResolvingService = "systemd"; };
  # };

  ## Profiles

  profiles = {
    desktop.sway.enable = false;
    desktop.wayland.enable = false;
    login.greetd.enable = false;
    media.pulseaudio.enable = false;
    media.pipewire.enable = true;
    net.tailscale.enable = false;
  };

  programs.dconf.enable = true;

  ## Display

  # TODO: configuration

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "session";
  services.xserver.displayManager.session = [
    {
      manage = "desktop";
      name = "session";
      start = ''exec $HOME/.xsession'';
    }
  ];
  services.xserver.libinput = {
    enable = true;
    touchpad.tapping = false;
    touchpad.scrollMethod = "twofinger";
    touchpad.clickMethod = "clickfinger";
  };

  ## Nix
  # TODO: use network.nix
  nix.distributedBuilds = false;
  # nix.buildMachines = [{
  #   hostName = "100.100.28.13";
  #   maxJobs = 8;
  #   sshUser = "root";
  #   system = "x86_64-linux";
  #   sshKey = "/root/.ssh/id_ed25519";
  #   supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  # }];
  # nix.extraOptions = ''
  #   builders-use-substitutes = true
  # '';

  ## Audio

  ## Security

  security.pam.loginLimits = [{
    domain = "*";
    type = "-";
    item = "nofile";
    value = "1048576";
  }];

  security.sudo = {
    package = pkgs.sudo.override { withInsults = true; };
    extraConfig = ''
      Defaults insults
    '';
  };

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
    storageDriver = "zfs";
  };

  # Guix
  users.extraUsers = let
    buildUser = (i: {
      "guixbuilder${i}" = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user ${i}";
        isSystemUser = true;
      };
    });
  in pkgs.lib.fold (str: acc: acc // buildUser str) { }
  (map (pkgs.lib.fixedWidthNumber 2) (builtins.genList (n: n + 1) 10));

  users.extraGroups.guixbuild.name = "guixbuild";
  environment.extraInit =
    ''
# _GUIX_PROFILE: `guix pull` profile
_GUIX_PROFILE="$HOME/.config/guix/current"
export PATH="$_GUIX_PROFILE/bin''${PATH:+:}$PATH"
export INFOPATH="$_GUIX_PROFILE/share/info:$INFOPATH"

# GUIX_PROFILE: User's default profile
GUIX_PROFILE="$HOME/.guix-profile"
if [ -L $GUIX_PROFILE ]; then
  GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
  export GUIX_LOCPATH

  [ -f "$GUIX_PROFILE/etc/profile" ] && . "$GUIX_PROFILE/etc/profile"

  # set XDG_DATA_DIRS to include Guix installations
  export XDG_DATA_DIRS="$GUIX_PROFILE/share:''${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
fi
'';
  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart =
        "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      Environment = "GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profile/lib/locale";
      RemainAfterExit = "yes";
      StandardOutput = "syslog";
      StandardError = "syslog";
      TaskMax = "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };

  programs.adb.enable = true;

  # User
  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "docker" "podman" "adbusers" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
    shell = pkgs.fish;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        cc.enable = true;
        common-lisp.enable = false;
        go.enable = true;
        python.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      desktop.sway.enable = false;
      editor = {
        emacs.enable = false;
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
      rofi
      mako
      nixos-option
      discord
      slack
      foot
      docker-compose
      postgresql
      yarn
      font-awesome
      gitlab-ci-lint
      jq
      yaml-language-server
      xdg_utils
      _1password-gui
      grim
      slurp
      wl-clipboard
      zathura
      texlive.combined.scheme-full
      plantuml
      figlet
      ditaa
      openjdk
      insomnia
      httpie
      spotify
      htmlTidy
      wally-cli
      gnumake
      nvtop
      intel-gpu-tools
      xlockmore
    ];

    gtk = {
      enable = true;
      gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
      font = {
        package = pkgs.roboto;
        name = "Roboto";
        size = 10;
      };
      # iconTheme = {
      #   name = "Papirus-Dark";
      #   package = pkgs.papirus-icon-theme;
      # };
      # theme = {
      #   name = "Orchis-dark";
      #   package = pkgs.orchis-theme;
      # };
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
