{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.kernelModules = ["vhost_vsock"];

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';

  virtualisation.libvirtd.enable = true;


  ## Networking

  networking.hostId = "99b75f29";
  networking.hostName = "buri";
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

  hardware.bluetooth = {
    enable = true;
    settings = { General = { ControllerMode = "dual"; }; };
  };

  services.xserver.videoDrivers = [ "nvidia" "modesetting" ];
  services.usbmuxd.enable = true;

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

  virtualisation.lxd.enable = true;
  virtualisation.lxd.package = pkgs.lxd.override { useQemu = true; };
  virtualisation.lxd.recommendedSysctlSettings = true;
  virtualisation.lxc.defaultConfig = "lxc.include = ''${pkgs.lxcfs}/share/lxc/config/common.conf.d/00-lxcfs.conf";
  virtualisation.lxc.lxcfs.enable = true;

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
    desktop = {
      flatpak.enable = true;
      libinput.enable = true;
      nvidia.enable = true;
      tlp.enable = true;
      xmonad.enable = true;
    };
    media.pipewire.enable = true;
  };

  programs.dconf.enable = true;

  ## Display

  services.xserver.displayManager.lightdm.enable = true;

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
        shell = pkgs.shadow;
        description = "Guix build user ${i}";
        isSystemUser = true;
      };
    });
  in pkgs.lib.fold (str: acc: acc // buildUser str) { }
  (map (pkgs.lib.fixedWidthNumber 2) (builtins.genList (n: n + 1) 10));

  users.extraGroups.guixbuild.name = "guixbuild";
  environment.extraInit = ''
    # GUIX_PROFILE: User's default profile
    GUIX_PROFILE="$HOME/.guix-profile"
    if [ -L $GUIX_PROFILE ]; then
      GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
      export GUIX_LOCPATH

      [ -f "$GUIX_PROFILE/etc/profile" ] && . "$GUIX_PROFILE/etc/profile"

      # set XDG_DATA_DIRS to include Guix installations
      export XDG_DATA_DIRS="$GUIX_PROFILE/share:''${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
    fi

    # _GUIX_PROFILE: `guix pull` profile
    _GUIX_PROFILE="$HOME/.config/guix/current"
    export PATH="$_GUIX_PROFILE/bin''${PATH:+:}$PATH"
    export INFOPATH="$_GUIX_PROFILE/share/info:$INFOPATH"
  '';
  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart =
        "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild --substitute-urls 'https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.gnu.org'";
      Environment =
        "GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profile/lib/locale";
      RemainAfterExit = "yes";
      StandardOutput = "syslog";
      StandardError = "syslog";
      TaskMax = "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };

  programs.adb.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;
  environment.systemPackages = [ pkgs.qemu ];

  services.transmission = {
    enable = false;
    openPeerPorts = true;
    settings = {
      encryption = 2;
      peer-port-random-on-start = true;
      peer-port-random-low = 50000;
    };
    extraFlags = [ "--log-debug" ];
  };

  # User
  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups =
      [ "wheel" "docker" "podman" "adbusers" "shadow" "kvm" "libvirt" "lxd" "video" "input" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
    shell = pkgs.zsh;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        cc.enable = true;
        common-lisp.enable = false;
        go.enable = true;
        haskell.enable = true;
        python.enable = false;
        nix.enable = true;
        rust.enable = true;
      };
      desktop = {
        xmonad.enable = true;
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
          asdf = true;
        };
        fish.enable = true;
        git.enable = true;
        gh.enable = false;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    # TODO: remove this and split in profile
    home.packages = with pkgs; [
      rofi
      nixos-option
      slack
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
      html-tidy
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
    };

    home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

    # TODO: refactor me
    programs.git = {
      extraConfig = {
        user.signingKey = "39035CC0B75D1142";
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
            email = "bastien.riviere@doctolib.com";
            signingKey = "AAD667ECD29B732B";
          };
          gitlab.user = "bastien.riviere";
          commit.gpgSign = true;
        };
      }];
    };

    programs.gpg.enable = true;

    programs.ssh.extraConfig = builtins.readFile ./ssh.conf;

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      defaultCacheTtl = 1800;
    };

    home.stateVersion = "22.05";
  };
}
