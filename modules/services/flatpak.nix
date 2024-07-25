{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.flatpak;
in
{
  options.modules.services.flatpak.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.flatpak = enabled;
    xdg.portal = enabled // {
      xdgOpenUsePortal = true;
    };

    # user.packages = with pkgs; [gnome.gnome-software];

    environment.sessionVariables.XDG_DATA_DIRS = [ "/var/lib/flatpak/exports/share" ];

    home.file.".local/share/flatpak/overrides/global".text = ''
      [Context]
      filesystems=/run/current-system/sw/share/X11/fonts:ro;/nix/store:ro
    '';
  };
}
