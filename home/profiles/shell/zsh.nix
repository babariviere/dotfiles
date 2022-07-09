{ config, inputs, lib, pkgs, ... }:

{
  programs.zsh = {
    # TODO: write configuration here
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;

    autocd = true;
    defaultKeymap = "emacs";
    history = {
      expireDuplicatesFirst = true;
      share = false;
    };

    initExtraFirst = let
      files = map (f: "${config.dotfiles.configDir}/zsh/${f}")
        ["init.zsh" "completion.zsh" "prompt.zsh" "config.zsh" "fini.zsh"];
      configs = map builtins.readFile files;
    in
      lib.concatStringsSep "\n" configs;

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
      {
        name = "zsh-z";
        src = "${pkgs.zsh-z}/share/zsh-z";
      }
    ];
  };
}
