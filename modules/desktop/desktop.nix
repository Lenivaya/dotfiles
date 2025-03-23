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

  autoRepeatDelay = 200;
  autoRepeatInterval = 100;
in
{
  options.modules.desktop = {
    enable = mkBoolOpt false;
    isWayland = mkBoolOpt false;
    isPureWM = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = enabled // {
      inherit autoRepeatDelay autoRepeatInterval;

      tty = mkDefault 1;

      xkb.layout = comcat [
        "us"
        "ru"
        "ua"
      ];
      xkb.options = comcat [
        "grp:win_space_toggle"
        "caps:ctrl_modifier"
      ];

      exportConfiguration = true;

      displayManager.lightdm = enabled // {
        greeters.gtk = enabled // {
          theme.package = pkgs.adw-gtk3;
          theme.name = "adw-gtk3-dark";
          iconTheme.package = pkgs.papirus-icon-theme;
          iconTheme.name = "Papirus-Dark";
        };
      };
      # displayManager.gdm.enable = mkDefault true;
      # https://github.com/NixOS/nixpkgs/issues/22164#issuecomment-394211867
      displayManager.sessionCommands = ''
        ${getExe pkgs.xorg.xset} r rate ${toString autoRepeatDelay} ${toString autoRepeatInterval}
      '';
    };

    services.dbus.packages = with pkgs; [ dconf ];
    programs.dconf = enabled;

    xdg = {
      portal = enabled // {
        extraPortals = with pkgs; [
          xdg-desktop-portal-gtk
          # xdg-desktop-portal-gnome
        ];
        # gtkUsePortal = true;
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
