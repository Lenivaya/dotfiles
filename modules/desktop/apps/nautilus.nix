{
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  services.gvfs = enabled;
  user.packages = with pkgs; [
    gnome.nautilus
    gnome.sushi

    nautilus-open-any-terminal
  ];
}
