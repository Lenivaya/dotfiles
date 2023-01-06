{
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  user.packages = with pkgs; [
    gnome.nautilus

    gnome.sushi
    nautilus-open-any-terminal
  ];
}
