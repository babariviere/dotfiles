{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.bitwarden;

  bw = pkgs.writeShellScriptBin "bw" ''
    if [ ! -r /tmp/.bw-session ]; then
       bw unlock --raw > /tmp/.bw-session
       chmod 400 /tmp/.bw-session
    fi
    session=$(head -n 1 /tmp/.bw-session)
    BW_SESSION="$session" ${pkgs.bitwarden-cli}/bin/bw $@
  '';
in {
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ bitwarden bw ];
  };
}
