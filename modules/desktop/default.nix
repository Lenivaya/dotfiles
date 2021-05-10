{ config, options, lib, pkgs, home-manager, ... }:

with lib;
with lib.my; {
  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = lib.mkDefault false;
    autoRepeatDelay = 200;
    autoRepeatInterval = 50;
    libinput = {
      scrollMethod = "edge";
      touchpad.disableWhileTyping = true;
      enable = true;
    };
    layout = "us, ru, ua";
    xkbOptions = "grp:win_space_toggle, caps:ctrl_modifier";

    displayManager.lightdm = {
      enable = true;
      greeters.gtk.theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome3.gnome_themes_standard;
      };
    };

  };
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  programs.dconf.enable = true;

  home-manager.users.${config.user.name}.services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Escape" = "pkill -USR1 -x sxhkd";

      # screencast region to mp4
      "super + Print" = "scrrec -s ~/recordings/$(date +%F-%T).mp4";
      # screencast region to gif
      "super + ctrl + Print" = "scrrec -s ~/recordings/$(date +%F-%T).gif";

      "super + KP_Left" = "st -e ranger";
      "super + shift + KP_Left" = "st -e nnn";
      "super + KP_Home" = "st -e tmux";

      # media keys
      "XF86Audio{LowerVolume,RaiseVolume,Mute}" = "volumedunst {down,up,mute}";
    };
  };

  # Clean up leftovers, as much as we can
  system.userActivationScripts.cleanupHome = ''
    pushd "${config.user.home}"
    rm -rf .compose-cache .nv .pki .dbus .fehbg
    [ -s .xsession-errors ] || rm -f .xsession-errors*
    popd
  '';
}
