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
      eval "$(${flow} setup $HOME/src --path ${flow} zsh)"
    '';

    completionInit = ''
      autoload -Uz compinit
      for dump in ~/.zcompdump(N.mh+24); do
        compinit
      done
      compinit -C
    '';

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
