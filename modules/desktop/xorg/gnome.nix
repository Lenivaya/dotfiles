{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.gnome;
in
{
  options.modules.desktop.gnome = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = enabled // {
      displayManager.gdm.enable = mkForce true;
      displayManager.lightdm.enable = mkForce false;
      desktopManager.gnome = enabled;
    };

    environment.systemPackages =
      with pkgs;
      [ gnome-tweaks ]
      ++ (with pkgs.gnomeExtensions; [
        # pop-shell
        # gesture-improvements
        gsconnect
        paperwm
        # vertical-workspaces # overview-navigation
        space-bar
        vim-alt-tab
        # useless-gaps
      ]);
    services.udev.packages = with pkgs; [ gnome-settings-daemon ];

    services.power-profiles-daemon.enable = false;
    services.pulseaudio.enable = false;
  };
}
