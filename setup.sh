#!/bin/sh

echo "Notice: If you are running on NixOS, you may need to run this command as root."
while true; do
    read -p "Continue? [y/n] " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done
echo "==> Adding home-manager channel"
nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager

echo "==> Adding unstable channel"
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable

echo "==> Updating all channels"
nix-channel --update
