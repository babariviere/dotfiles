{ config, lib, options, pkgs, ... }:

let cfg = config.my.services.tailscale;
in with pkgs; {
  options.my.services.tailscale = with lib; {
    enable = mkEnableOption "tailscale";
  };

  # HACK: split it up into two file? or find a proper way to do this

  config = lib.mkIf cfg.enable (lib.mkMerge [
    (lib.optionalAttrs (options ? services.tailscale) {
      services.tailscale = { enable = true; };
    })
    (lib.optionalAttrs (options ? launchd) {
      environment.systemPackages = [ pkgs.tailscale ];
      launchd.daemons.tailscaled = {
        serviceConfig.ProgramArguments = [ "${pkgs.tailscale}/bin/tailscaled" ];
        serviceConfig.RunAtLoad = true;
        serviceConfig.StandardErrorPath = "/var/log/tailscale.log";
      };
    })
  ]);

}
