{
  domain = "home";
  nodes = [
    {
      name = "ochatt";
      ip = "100.78.240.51";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKdGuXrkHZdLl/+/kTxtbByYyxhLpcMTzfx3fABK6qt4";
    }
    {
      name = "vercar";
      ip = "100.100.28.13";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMO6/jzKplTgI8UEbJTdNskExUfQEJ9GG1b9smYXoaP4";
    }
    {
      name = "beehum";
      ip = "100.89.219.104";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAR8+m6kSrZ8Zc3+NMolR2mzmfgzFPhij/cGl2KGHDBQ";
    }
  ];
}
