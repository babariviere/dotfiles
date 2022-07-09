{ config, lib, options, ... }:

lib.optionalAttrs (options.networking ? domain) {
  security.acme = {
    server =
      "https://${config.networking.hostName}.${config.networking.domain}:${
        toString config.services.step-ca.port
      }/acme/acme/directory";
    email = "root@${config.networking.hostName}.${config.networking.domain}";
    acceptTerms = true;
  };

  # Certs generated with:
  # 
  # step certificate create --profile root-ca "Example Root CA" root_ca.crt root_ca.key
  # step certificate create "Example Intermediate CA 1" intermediate_ca.crt intermediate_ca.key --profile intermediate-ca --ca ./root_ca.crt --ca-key ./root_ca.key

  services.step-ca = {
    enable = true;
    address = "0.0.0.0";
    port = 8443;
    intermediatePasswordFile = config.age.secrets."step-ca/password".path;
    settings = {
      dnsNames =
        [ "${config.networking.hostName}.${config.networking.domain}" ];
      root = config.dotfiles.dir
        + "/secrets/${config.networking.hostName}/step-ca/root_ca.crt";
      crt = config.dotfiles.dir
        + "/secrets/${config.networking.hostName}/step-ca/intermediate_ca.crt";
      key = config.age.secrets."step-ca/intermediate_ca.key".path;
      db = {
        type = "badger";
        dataSource = "/var/lib/step-ca/db";
      };
      authority = {
        provisioners = [{
          type = "ACME";
          name = "acme";
        }];
      };
    };
  };

  security.pki.certificateFiles = [
    (/. + config.dotfiles.dir
      + "/secrets/${config.networking.hostName}/step-ca/root_ca.crt")
  ];

  users.groups.step-ca.name = "step-ca";

  systemd.services.step-ca = let
    certs = map (cert: "acme-${cert}.service")
      (builtins.attrNames config.security.acme.certs);
  in {
    requiredBy = certs;
    serviceConfig = {
      SupplementaryGroups = [ config.users.groups.step-ca.name ];
    };
  };
}
