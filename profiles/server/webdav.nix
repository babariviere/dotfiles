{ config, lib, options, pkgs, ... }:

let domain = "dav.${config.networking.hostName}.${config.networking.domain}";
in lib.optionalAttrs (options.networking ? domain) {
  services.nginx = {
    additionalModules = [ pkgs.nginxModules.dav ];
    appendHttpConfig = ''
      dav_ext_lock_zone zone=${config.networking.hostName}:10m;
    '';
    virtualHosts."${domain}" = {
      root = "/srv/dav";
      extraConfig = ''
        dav_methods PUT DELETE MKCOL COPY MOVE;
        dav_access user:rw;
        dav_ext_methods PROPFIND OPTIONS LOCK UNLOCK;
        dav_ext_lock zone=${config.networking.hostName};

        auth_basic "Password";
        auth_basic_user_file ${config.age.secrets."nginx-auth-file".path};

        autoindex on;

        client_body_temp_path   /tmp/nginx-client-bodies;
        client_max_body_size    0;
        create_full_put_path    on;
      '';
    };
  };
}
