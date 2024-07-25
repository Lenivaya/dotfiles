let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKJWZChgyuytRTYKH/TkMY0rS7QTUXzdlSfa0SRSrqK/ xocada@gmail.com";
in
{
  "your_spotify_secret.age".publicKeys = [ key ];
}
