{ config, lib, options, ... }:

lib.optionalAttrs (options ? services.tailscale) {
  services.tailscale.enable = true;
  networking.firewall.allowedTCPPorts = lib.mkForce [ ];
  networking.firewall.allowedUDPPorts =
    lib.mkForce [ config.services.tailscale.port ];
  networking.firewall.trustedInterfaces =
    [ config.services.tailscale.interfaceName ];
}
