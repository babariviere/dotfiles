{ pkgs, ... }:

{
  # TODO: separate packages?
  home.packages = with pkgs; [ age bat cachix exa dogdns sd fd ripgrep ];

  programs.exa = {
    enable = true;
    enableAliases = true;
  };
  programs.fzf.enable = true;
  programs.nix-index.enable = true;
  programs.ssh.enable = true;

  shell.aliases = {
    gco = "git co";
    gs = "git s";

    dup = "docker-compose up";
    ddn = "docker-compose down";

    dr = "darwin-rebuild";
    drs = "darwin-rebuild switch --flake . --keep-going";

    nr = "sudo nixos-rebuild";
    nrs = "sudo nixos-rebuild switch --flake . --keep-going";

    wk = "watch kubectl";
    # FIXME: move me to kube profile
    k = "${pkgs.kubectl}/bin/kubectl";
    kns = "${pkgs.kubectx}/bin/kubens";
    kctx = "${pkgs.kubectx}/bin/kubectx";
  };
}
