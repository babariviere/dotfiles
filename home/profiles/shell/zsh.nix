{ config, inputs, lib, pkgs, ... }:

{
  programs.zsh = {
    # TODO: write configuration here
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;

    autocd = true;
    defaultKeymap = "viins";
    history = {
      expireDuplicatesFirst = true;
      share = false;
    };

    initExtra = let
      flow = "${inputs.flow.defaultPackage.${pkgs.system}}/bin/flow";
      base = builtins.readFile "${config.dotfiles.configDir}/zshrc";
    in ''
      ${base}
      eval "$(${flow} setup $HOME/src --path ${flow})"
    '';

    completionInit = ''
      autoload -Uz compinit
      for dump in ~/.zcompdump(N.mh+24); do
        compinit
      done
      compinit -C
    '';

    shellAliases = {
      ls = "${pkgs.exa}/bin/exa";
      ll = "ls -l";
      l = "ls";

      gco = "git co";
      gs = "git s";

      dup = "docker-compose up";
      ddn = "docker-compose down";

      dr = "darwin-rebuild";
      drs = "darwin-rebuild switch --flake . --keep-going";

      wk = "watch kubectl";
      k = "${pkgs.kubectl}/bin/kubectl";
      kns = "${pkgs.kubectx}/bin/kubens";
      kctx = "${pkgs.kubectx}/bin/kubectx";
    };

    plugins = [
      {
        name = "fast-syntax-highlighting";
        src = "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions";
      }
      {
        name = "zsh-history-substring-search";
        file = "zsh-history-substring-search.zsh";
        src =
          "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search";
      }
    ];
  };
}
