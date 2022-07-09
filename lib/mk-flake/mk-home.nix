{ home, inputs }:


host:
  inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;

    modules = (home.modules or []) ++ (home.profiles or []) ++ host.modules;
    extraSpecialArgs = { inherit inputs; };
  }
