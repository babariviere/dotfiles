{ config, pkgs }:

{
  boot.blacklistedKernelModules = [ "nouveau" ];

  services.xserver.dpi = 96;

  hardware.opengl.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  services.xserver.videoDrivers = [ "nvidia" ];
}
