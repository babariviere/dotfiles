{ config, lib, pkgs, inputs, ... }: {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "${config.meta.specie.code}";
  networking.domain = "home";
  services.tailscale.enable = true;
  networking.firewall.allowedTCPPorts = lib.mkForce [ ];
  networking.firewall.allowedUDPPorts =
    lib.mkForce [ config.services.tailscale.port ];
  networking.firewall.trustedInterfaces =
    [ config.services.tailscale.interfaceName ];

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp82s0.useDHCP = true;
  networking.hostId = "461aad11";
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlp82s0" ];

  environment.systemPackages = with pkgs; [ neovim curl wget ];

  virtualisation.docker.enable = false;
  users.users.docker = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "docker" ];
    description = "User with only docker access";
  };
  services.logind.lidSwitch = "ignore";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.openFirewall = false;

  services.unbound = {
    enable = true;
    localControlSocketPath = "/run/unbound/unbound.ctl";
    settings = {
      server = let
        ads = pkgs.runCommand "ads.conf" { } ''
          cat ${inputs.hosts-denylist}/alternates/fakenews-gambling-porn/hosts | ${pkgs.gnugrep}/bin/grep '^0\.0\.0\.0' | ${pkgs.gawk}/bin/awk '{print "local-zone: \""$2"\" redirect\nlocal-data: \""$2" A 0.0.0.0\""}' > $out
        '';
      in {
        # TODO: use `network.nix` for IPs
        # NOTE: libvirt allocate port 53 for dnsmasq so we can't use port 53
        interface = [ "127.0.0.1" "100.100.28.13" ];
        access-control = [ "127.0.0.0/8 allow" "100.0.0.0/8 allow" ];
        # port = 5335;

        log-queries = true;
        statistics-interval = 0;
        extended-statistics = true;
        statistics-cumulative = true;

        do-ip4 = true;
        do-ip6 = true;
        do-udp = true;
        do-tcp = true;

        # You want to leave this to no unless you have *native* IPv6. With 6to4 and
        # Terredo tunnels your web browser should favor IPv4 for the same reasons
        prefer-ip6 = false;

        # Trust glue only if it is within the server's authority
        harden-glue = true;

        # Require DNSSEC data for trust-anchored zones, if such data is absent, the zone becomes BOGUS
        harden-dnssec-stripped = true;

        # Don't use Capitalization randomization as it known to cause DNSSEC issues sometimes
        # see https://discourse.pi-hole.net/t/unbound-stubby-or-dnscrypt-proxy/9378 for further details
        use-caps-for-id = false;

        # Reduce EDNS reassembly buffer size.
        # Suggested by the unbound man page to reduce fragmentation reassembly problems
        edns-buffer-size = 1472;

        # Perform prefetching of close to expired message cache entries
        # This only applies to domains that have been frequently queried
        prefetch = true;

        # Ensure kernel buffer is large enough to not lose messages in traffic spikes
        so-rcvbuf = "1m";

        # Ensure privacy of local IP ranges
        private-address = [
          "192.168.0.0/16"
          "169.254.0.0/16"
          "172.16.0.0/12"
          "10.0.0.0/8"
          "fd00::/8"
          "fe80::/10"
        ];

        # TODO: automate this
        #
        # redirect all queries from ${hostname}.home to the correct ip address.
        # local-zone redirect allow redirection of all subdomain
        # local-data set the ip for the domain
        local-zone =
          [ ''"vercar.home." redirect'' ''"ochatt.home." redirect'' ];
        local-data = [
          ''"vercar.home. A 100.100.28.13"''
          ''"ochatt.home. A 100.78.240.51"''
        ];

        include = [ (toString ads) ];
      };
    };
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
      unbound = {
        enable = true;
        port = 9003;
        controlInterface = config.services.unbound.localControlSocketPath;
        group = config.services.unbound.group;
      };
    };

    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [{
          targets = [
            "127.0.0.1:${
              toString config.services.prometheus.exporters.node.port
            }"
          ];
        }];
      }
      {
        job_name = "unbound";
        static_configs = [{
          targets = [
            "127.0.0.1:${
              toString config.services.prometheus.exporters.unbound.port
            }"
          ];
        }];
      }
    ];
    # TODO(babariviere): check if we can add more config
  };

  services.grafana = {
    enable = true;
    port = 8120;
    addr = "127.0.0.1";
    domain =
      "grafana.${config.networking.hostName}.${config.networking.domain}";

    declarativePlugins = with pkgs.grafanaPlugins; [ grafana-piechart-panel ];

    # TODO: security.adminPasswordFile and secretKeyFile
    # A good occasion to test agenix
    provision = {
      enable = true;
      datasources = [{
        name = "Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "http://localhost:${toString config.services.prometheus.port}";
      }];
      dashboards = [
        {
          name = "Node Full Exporter";
          disableDeletion = true;
          options.path = /. + config.dotfiles.configDir
            + "/grafana/dashboards/node_full_exporter.json";
        }
        {
          name = "Unbound";
          disableDeletion = true;
          options.path = /. + config.dotfiles.configDir
            + "/grafana/dashboards/unbound.json";
        }
      ];
    };
  };

  services.nginx.enable = true;
  services.nginx.virtualHosts.${config.services.grafana.domain} = {
    locations."/" = {
      proxyPass = "http://${toString config.services.grafana.addr}:${
          toString config.services.grafana.port
        }";
      proxyWebsockets = true;
    };
  };
  services.nginx.virtualHosts."nix-store.${config.networking.hostName}.${config.networking.domain}" =
    {
      locations."/" = {
        proxyPass = "http://${toString config.services.nix-serve.bindAddress}:${
            toString config.services.nix-serve.port
          }";
      };
    };

  services.nix-serve = {
    enable = true;
    secretKeyFile = config.age.secrets."nix-serve".path;
  };

  age.secrets = let
    step-ca = file: {
      name = "step-ca/${file}";
      value = {
        # TODO: make it readable for pki too
        file = ../../secrets/vercar/step-ca + "/${file}.age";
        owner = "nobody";
        group = "step-ca";
        mode = "0040";
      };
    };
  in {
    "nix-serve" = {
      file = ../../secrets/vercar/nix-serve.age;
      owner = "nix-serve";
      group = "nogroup";
    };
  } // (builtins.listToAttrs
    (map step-ca [ "intermediate_ca.key" "root_ca.key" "password" ]));
  system.activationScripts.users.supportsDryActivation = lib.mkForce false;

  users.users.nix-serve = { isNormalUser = true; };

  # this is needed to get a bridge with DHCP enabled
  virtualisation.libvirtd.enable = true;

  # reboot your computer after adding those lines
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';

  profiles = {
    security = {
      step-ca.enable = false;
      vaultwarden.enable = false;
    };
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  meta = {
    specie = {
      url = "https://ebird.org/species/vercar1";
      code = "vercar";
      name = "Vermilion Cardinal";
    };
  };
}
