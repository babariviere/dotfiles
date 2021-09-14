{ config, lib, options, ... }:

let
  dataFolder = "/var/lib/vaultwarden";
  domain = "bw.${config.networking.hostName}.${config.networking.domain}";
in lib.optionalAttrs (options.networking ? domain) {
  services.vaultwarden = {
    enable = true;
    backupDir = "${dataFolder}/backups";
    config = {
      domain = "https://${domain}";
      dataFolder = dataFolder;
      rocketAddress = "127.0.0.1";
      rocketPort = 8520;
    };
  };

  systemd.services.backup-vaultwarden.environment.DATA_FOLDER =
    lib.mkForce dataFolder;

  systemd.services.vaultwarden.serviceConfig.StateDirectory =
    lib.mkForce "vaultwarden";

  services.nginx.virtualHosts."${domain}" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass =
        "http://${toString config.services.vaultwarden.config.rocketAddress}:${
          toString config.services.vaultwarden.config.rocketPort
        }";
      proxyWebsockets = true;
    };
  };
}
