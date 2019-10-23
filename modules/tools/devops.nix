{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    terraform packer ansible
  ];
}
