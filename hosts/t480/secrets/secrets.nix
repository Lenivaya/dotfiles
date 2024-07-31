let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPc9ioKn9tK8mbR2CNCCWO1+hTdYC1gE+nUi9lS6TzsL leniviy@t480";
  key2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMA+TLuT5zgPZCPsJGrR1kvnrDPu7/oj8WQWdsfmVZ51 root@t480";
in
{
  "nextdns.age".publicKeys = [
    key
    key2
  ];
}
