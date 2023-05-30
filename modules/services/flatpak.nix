{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.flatpak;
in {
  options.modules.services.flatpak.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    xdg.portal.enable = true;
    home.file.".local/share/flatpak/overrides/global".text = ''
      [Context]
      filesystems=/run/current-system/sw/share/X11/fonts:ro;/nix/store:ro
    '';
  };
}
