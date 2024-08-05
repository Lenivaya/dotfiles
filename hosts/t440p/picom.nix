{ config, lib, ... }:
with lib;
with my;
let
  inherit (config.dotfiles) configDir;
in
{
  # FIXME HACK unless animations are properly supported by home-manager
  home.configFile."picom/picom.conf".source = mkForce "${configDir}/picom/picom.conf";

  # home.services.picom.vSync = mkForce false;
  # home.services.picom.backend = mkForce "xrender";
  # home.services.picom.settings = let
  #   animationExclude = [
  #     "class_g *= 'xmobar'"
  #     "class_g *= 'xmonad'"
  #     "class_g *= 'xmonad-prompt'"
  #     "name *= 'xmobar'"
  #     "name *= 'xmonad'"
  #     "name *= 'xmonad-prompt'"
  #     "class_g *= 'slop'"
  #     "name *= 'slop'"
  #     "class_g *= 'skippy-xd'"
  #     "class_g *= 'skippy-xd'"
  #     "class_g *= 'safeeyes'"
  #   ];
  # in {
  #   corner-radius = 0;

  #   animations = false;
  #   # animation-stiffness = 100;
  #   # animation-window-mass = 0.8;
  #   # animation-dampening = 10;
  #   # animation-clamping = true;

  #   animation-open-exclude = animationExclude;
  #   animation-unmap-exclude = animationExclude;

  #   inactive-exclude = [
  #     "window_type = 'dock'"
  #     "window_type = 'desktop'"
  #     "window_type = 'menu'"
  #     "window_type = 'popup_menu'"
  #     "window_type = 'dropdown_menu'"
  #     "window_type = 'toolbar'"
  #     "window_type = 'notification'"
  #     "class_g *= 'avizo-service'"
  #     "class_g *= 'slop'"
  #     "name *= 'slop'"
  #   ];
  # };
}
