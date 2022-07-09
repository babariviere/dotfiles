{ config, lib, pkgs, ... }:

{
  programs.gh = {
    enable = true;
    settings = {
      aliases = {
        pc = "pr create";
        pcd = "pr create -d";
      };
      git_protocol = "ssh";
    };
  };
}
