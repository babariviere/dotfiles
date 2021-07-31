{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.git
    pkgs.git-ignore
    pkgs.git-fame
    pkgs.git-open
    pkgs.git-trim
    pkgs.gitleaks
    pkgs.github-cli
  ];

  programs.git = {
    enable = true;
    aliases = {
      cleanup = "trim";
      co = "checkout";
      s = "status --branch --short";
    };
    attributes = [
      # TODO:
      # "*.age diff=age"
    ];
    delta = {
      enable = true;
      options = {
        features = "line-numbers decorations";
        syntax-theme = "Dracula";
        decorations = {

          commit-decoration-style = "none";
          file-style = "yellow bold ul";
          file-decoration-style = "black bold ol";
          hunk-header-decoration-style = "magenta box";
        };
        line-numbers = {
          line-numbers-minus-style = "#444444";
          line-numbers-zero-style = "#444444";
          line-numbers-plus-style = "#444444";
          line-numbers-left-style = "magenta";
          line-numbers-right-style = "magenta";
        };
      };
    };
    extraConfig = {
      core = {
        autocrlf = false;
        eof = "lf";
      };
      init.defaultBranch = "main";
      github.user = "babariviere";
    };
    ignores = [
      ".envrc"
      ".lsp"
      ".rebel_readline_history"
      ".projectile"
      "*.pem"
      "*.swp"
      ".DS_Store"
    ];
    includes = [{ path = "~/.gitconfig.local"; }];
    userEmail = "babathriviere@gmail.com";
    userName = "Bastien Riviere";
  };
}
