{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gnome;
in {
  options.modules.desktop.gnome = {enable = mkBoolOpt false;};

  config = mkIf cfg.enable {
    services.xserver =
      enabled
      // {
        displayManager.gdm.enable = mkForce true;
        desktopManager.gnome = enabled;
      };

    environment.systemPackages = with pkgs;
      [gnome.gnome-tweaks]
      ++ (with pkgs.gnomeExtensions; [
        pop-shell
        gesture-improvements
        gsconnect
        overview-navigation
        space-bar
        vim-alt-tab
        # useless-gaps
      ]);
    services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];

    services.power-profiles-daemon.enable = false;
    hardware.pulseaudio.enable = false;
  };
}
