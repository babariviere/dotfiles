{ home, inputs, lib, self }:


host:
  inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = lib.attrValues self.overlays;
      config.allowUnfree = true;
    };

    modules = (home.modules or []) ++ (home.profiles or []) ++ host.modules;
    extraSpecialArgs = { inherit inputs; };
  }
