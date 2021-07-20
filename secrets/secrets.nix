let
  ochatt =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKdGuXrkHZdLl/+/kTxtbByYyxhLpcMTzfx3fABK6qt4";
  users = [ ochatt ];

  vercar =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILxFkInHWWR3JeTA/4mzZdzrKwH6AAnvGB4jnhARfUn1";
  servers = [ vercar ];
in { "vercar.nix-serve.age".publicKeys = [ vercar ]; }
