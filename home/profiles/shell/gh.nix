{ config, lib, pkgs, ... }:

{

  programs.gh = {
    enable = true;
    aliases = {
      pc = "pr create";
      pcd = "pr create -d";
    };
    gitProtocol = "ssh";
  };
}
