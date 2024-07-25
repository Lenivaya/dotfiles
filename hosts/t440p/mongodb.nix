{ lib, pkgs, ... }:
with lib;
with lib.my;
{
  user.packages = with pkgs; [
    mongosh
    mongodb-tools
    mongodb-compass
  ];

  services = {
    # mongodb = enabled // {};
    # let it be docker.
  };
}
