let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKJWZChgyuytRTYKH/TkMY0rS7QTUXzdlSfa0SRSrqK/";
  key2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFd9g1in9f8odPZ7GakncXW1rYumYrRz4x5DqnyVgvFd root@t440p";
in {
  "nextdns.age".publicKeys = [key key2];
}
