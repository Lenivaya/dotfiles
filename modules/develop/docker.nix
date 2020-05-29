{ config, lib, ... }:

{
  virtualisation.docker.enableOnBoot = false;
  virtualisation.docker.enable = true;
}
