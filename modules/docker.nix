{ config, lib, ... }:

{
  virtualisation.docker.enableOnBoot = lib.mkForce false;
  virtualisation.docker.enable = true;
}
