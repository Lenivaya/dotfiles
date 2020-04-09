{ config, lib, pkgs, ... }:

{
  imports = [ ./kdeconnect.nix ./common.nix ./ssh.nix ];
}
