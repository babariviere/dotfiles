let
  ochatt =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKdGuXrkHZdLl/+/kTxtbByYyxhLpcMTzfx3fABK6qt4";
  users = [ ochatt ];

  vercar =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMO6/jzKplTgI8UEbJTdNskExUfQEJ9GG1b9smYXoaP4";
  servers = [ vercar ];
in { "vercar.nix-serve.age".publicKeys = [ ochatt vercar ]; }
