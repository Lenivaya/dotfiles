{
  config,
  options,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  configDir = config.dotfiles.configDir;
in {
  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = lib.mkDefault false;
    autoRepeatDelay = 200;
    autoRepeatInterval = 50;
    libinput = {
      touchpad = {disableWhileTyping = true;};
      enable = true;
    };
    layout = "us, ru, ua";
    xkbOptions = "grp:win_space_toggle, caps:ctrl_modifier";

    displayManager.lightdm = {
      enable = true;
      greeters.gtk.theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome3.gnome-themes-extra;
      };
    };
  };

  services.dbus.packages = with pkgs; [dconf];
  programs.dconf.enable = true;

  # xdg = {
  #   portal = {
  #     enable = true;
  #     extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-gnome ];
  #     gtkUsePortal = true;
  #   };
  # };
  #
  home.configFile."sxhkd" = {
    source = "${configDir}/sxhkd";
    recursive = true;
  };

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.sxhkd}/bin/sxhkd -c ${configDir}/sxhkd/sxhkdrc &
    ${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1 &
  '';

  # Clean up leftovers, as much as we can
  system.userActivationScripts.cleanupHome = ''
    pushd "${config.user.home}"
    rm -rf .compose-cache .nv .pki .dbus .fehbg
    [ -s .xsession-errors ] || rm -f .xsession-errors*
    popd
  '';
}
