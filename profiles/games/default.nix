{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ steam steam-run ];
  services.xserver.libinput.disableWhileTyping = lib.mkForce false;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  environment.sessionVariables = { WINEDEBUG = "-all"; };
}
