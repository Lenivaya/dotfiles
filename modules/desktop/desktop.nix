{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop;
in
{
  options.modules.desktop = {
    enable = mkBoolOpt false;
    isWayland = mkBoolOpt false;
    isPureWM = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = enabled // {
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;
      layout = comcat [
        "us"
        "ru"
        "ua"
      ];
      xkbOptions = comcat [
        "grp:win_space_toggle"
        "caps:ctrl_modifier"
      ];

      exportConfiguration = true;

      displayManager.lightdm = mkDefault (
        enabled
        // {
          greeters.gtk.theme = {
            # package = pkgs.gnome.gnome-themes-extra;
            # name = "Adwaita-dark";
            package = pkgs.adw-gtk3;
            name = "adw-gtk3-dark";
          };
        }
      );
    };

    services.dbus.packages = with pkgs; [ dconf ];
    programs.dconf = enabled;

    xdg = {
      portal = enabled // {
        extraPortals = with pkgs; [
          xdg-desktop-portal-gtk
          # xdg-desktop-portal-gnome
        ];
        gtkUsePortal = true;
      };
    };

    modules.services.sxhkd = enabled;

    # Clean up leftovers, as much as we can
    system.userActivationScripts.cleanupHome = ''
      pushd "${config.user.home}"
      rm -rf .compose-cache .nv .pki .dbus
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';

    environment.shellAliases = {
      restartx = "systemctl restart display-manager.service";
    };
  };
}
