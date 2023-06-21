{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  services.xserver =
    enabled
    // {
      desktopManager.xterm.enable = mkDefault false;
      autoRepeatDelay = 200;
      autoRepeatInterval = 50;
      layout = comcat ["us" "ru" "ua"];
      xkbOptions = comcat ["grp:win_space_toggle" "caps:ctrl_modifier"];

      displayManager.lightdm =
        enabled
        // {
          greeters.gtk.theme = {
            name = "Adwaita-dark";
            package = pkgs.gnome3.gnome-themes-extra;
          };
        };
    };

  services.dbus.packages = with pkgs; [dconf];
  programs.dconf = enabled;

  # xdg = {
  #   portal = {
  #     = enabled;
  #     extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-gnome ];
  #     gtkUsePortal = true;
  #   };
  # };

  modules.services.sxhkd = enabled;

  # Clean up leftovers, as much as we can
  system.userActivationScripts.cleanupHome = ''
    pushd "${config.user.home}"
    rm -rf .compose-cache .nv .pki .dbus .fehbg
    [ -s .xsession-errors ] || rm -f .xsession-errors*
    popd
  '';
}
