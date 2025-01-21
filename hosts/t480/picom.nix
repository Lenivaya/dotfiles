{ config, lib, ... }:
with lib;
with my;
let
  inherit (config.dotfiles) configDir;
in
{
  # # FIXME HACK unless animations are properly supported by home-manager
  # home.configFile."picom/picom.conf".source = mkForce "${configDir}/picom/picom.conf";
  modules.desktop.compositor.enable = mkForce false;
}
# // disableUserService "picom"
