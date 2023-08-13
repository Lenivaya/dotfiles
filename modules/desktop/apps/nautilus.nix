{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my; {
  services.gvfs = enabled;
  user.packages = with pkgs.gnome;
    [
      nautilus
      nautilus-python
      sushi
      file-roller
      simple-scan
    ]
    ++ (with pkgs; [nautilus-open-any-terminal]);

  # Without this plugins won't work[1]
  # (when not using GNOME DE)
  # [1]: https://github.com/NixOS/nixpkgs/issues/168651#issuecomment-1373275164
  environment.pathsToLink = [
    "/share/nautilus-python/extensions"
  ];
  environment.sessionVariables.NAUTILUS_4_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-4";
}
