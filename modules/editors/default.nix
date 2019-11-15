{ config, lib, pkgs, ... }:

{
  imports = [ ./emacs ./neovim.nix ];
}
