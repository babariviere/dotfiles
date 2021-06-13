{ lib, ... }:

let modules = import ./modules.nix { inherit lib; };
in { inherit (modules) findModules findModulesRec; }
