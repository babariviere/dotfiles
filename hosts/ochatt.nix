{ config, inputs, lib, pkgs, ... }:

let
  # emacs = config.home-manager.users.bastienriviere.programs.emacs.package;
  emacs = pkgs.emacsOsx;
in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ emacs pkgs.hugo ];

  homebrew = {
    enable = true;
    autoUpdate = true;
    cleanup = "zap";
    global.brewfile = true;
    casks = [
      "1password"
      "battery-buddy"
      "dash"
      "discord"
      "docker"
      "encryptme"
      "gpg-suite-no-mail"
      "firefox"
      "insomnia"
      "iterm2"
      "postico"
      "raycast"
      "slack"
      "spotify"
      "wezterm"
      "zsa-wally"
    ];
    taps = [
      "fewlinesco/tap"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
    ];
    brews = [ "mas" "fewlinesco/tap/fwl_error" ];
    masApps = {
      "Spark" = 1176895641;
      "Tailscale" = 1475387142;
    };
  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";
  environment.pathsToLink = [ "/share/zsh" ];

  nix.trustedUsers = lib.attrNames config.users.users;
  nix.buildMachines = [{
    hostName = "100.100.28.13";
    maxJobs = 8;
    sshUser = "root";
    system = "x86_64-linux";
    sshKey = "/var/root/.ssh/id_rsa";
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  }];
  nix.distributedBuilds = true;
  # FIXME(babariviere): age module is not supported for darwin.
  # either:
  # - add missing `deps` attribute to nix-darwin
  # - create my own module to support secrets
  #
  # nix.extraOptions = let
  #   postBuildHook = pkgs.writeShellScript "post-build-hook.sh" ''
  #     set -eu
  #     set -f # disable globbing
  #     export IFS=' '

  #     echo "Signing paths" $OUT_PATHS
  #     nix store sign --key-file ${
  #       config.age.secrets."nix-serve".path
  #     } $OUT_PATHS
  #     echo "Uploading paths" $OUT_PATHS
  #     exec nix copy --to 'ssh://vercar' $OUT_PATHS
  #   '';
  # in ''
  #   post-build-hook = ${postBuildHook}
  # '';

  # age.sshKeyPaths =
  #   [ "${config.user.home}/.ssh/id_rsa" "${config.user.home}/.ssh/id_ed25519" ];
  # age.secrets = {
  #   "nix-serve" = {
  #     file = ../../secrets/vercar.nix-serve.age;
  #     owner = "root";
  #     group = "nogroup";
  #   };
  # };

  launchd.daemons = {
    limits = {
      script = ''
        launchctl limit maxfiles 524288 524288
        launchctl limit maxproc 8192 8192
      '';
      serviceConfig.RunAtLoad = true;
      serviceConfig.KeepAlive = true;
    };
  };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  programs.nix-index.enable = true;

  # system.activationScripts.applications.text = pkgs.lib.mkForce (''
  #   rm -rf ~/Applications/Nix\ Apps
  #   mkdir -p ~/Applications/Nix\ Apps
  #   for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
  #   src="$(/usr/bin/stat -f%Y "$app")"
  #   cp -r "$src" ~/Applications/Nix\ Apps
  #   done
  # '');

  # NOTE: I don't have system profiles (RIP)
  # profiles = { };

  home-manager.users.bastienriviere = {
    profiles = {
      dev = {
        common-lisp.enable = true;
        clojure.enable = true;
        elixir.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      editor = {
        # TODO: maybe to a flavor instead? maybe use a module then
        doom.enable = true;
        emacs.enable = false;
        editorconfig.enable = true;
      };
      ops = {
        aws.enable = true;
        kubernetes.enable = true;
      };
      shell = {
        common.enable = true;
        direnv = {
          enable = true;
          nix = true;
          asdf = true;
        };
        git.enable = true;
        gh.enable = true;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    home.packages = let
      nvimConfig = pkgs.neovimUtils.makeNeovimConfig {
        withPython3 = true;
        withNodeJs = true;
        viAlias = true;
        vimAlias = true;
      };
      nvim = pkgs.wrapNeovimUnstable pkgs.neovim-nightly (nvimConfig // {
        wrapperArgs = (lib.escapeShellArgs nvimConfig.wrapperArgs);
        wrapRc = false;
      });
    in [
      nvim
      pkgs.tree-sitter

      pkgs.tailscale

      # Tools
      pkgs.age
      pkgs.bat
      pkgs.exa
      pkgs.lunchy
    ];

    programs.exa.enable = true;
    programs.fish.enable = false;
    programs.fzf.enable = true;
    programs.nix-index.enable = true;
    programs.ssh.enable = true;

    home.stateVersion = "21.03";
  };

  users.users.bastienriviere.home = "/Users/bastienriviere";

  networking.hostName = "ochatt";
  # networking.dns = [ "100.100.28.13" "1.1.1.1" ];
  networking.knownNetworkServices = [ "Wi-Fi" ];

  system.defaults = {
    finder = {
      AppleShowAllExtensions = true;
      QuitMenuItem = true;
      _FXShowPosixPathInTitle = true;
    };
    NSGlobalDomain.AppleFontSmoothing = 0;
  };

  # TODO(babariviere): convert them to homemanager options
  system.activationScripts.myDefaults.text = ''
    defaults write com.apple.desktopservices DSDontWriteNetworkStores true
    defaults write com.apple.finder AppleShowAllFiles -boolean true
  '';

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  meta = {
    specie = {
      url = "https://ebird.org/species/ochatt1";
      code = "ochatt";
      name = "Ochraceous Attila";
    };
  };
}
