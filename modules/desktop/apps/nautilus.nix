{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my; let
  inherit (config.user) name;
in {
  config = mkIf config.modules.desktop.enable {
    services = {
      # mounting
      gvfs.enable = true;
      udisks2.enable = true;
      devmon.enable = true;

      # thumbnails
      tumbler.enable = true;

      dbus.packages = with pkgs; [
        nautilus-open-any-terminal
      ];
    };

    environment.systemPackages = with pkgs.gnome;
      [
        nautilus
        nautilus-python
        sushi
        file-roller
        simple-scan
      ]
      ++ (with pkgs; [
        nautilus-open-any-terminal

        # thumbnails
        gst_all_1.gst-libav
        ffmpegthumbnailer
      ]);

    services.xserver.desktopManager.gnome.extraGSettingsOverridePackages = with pkgs; [
      nautilus-open-any-terminal
    ];

    home-manager.users.${name}.dconf.settings = {
      "com/github/stunkymonkey/nautilus-open-any-terminal" = {
        keybindings = "'<Ctrl><Alt>t";
        terminal = config.modules.desktop.term.default;
      };
    };

    # Without this plugins won't work[1]
    # (when not using GNOME DE)
    # [1]: https://github.com/NixOS/nixpkgs/issues/168651#issuecomment-1373275164
    environment.pathsToLink = [
      "/share/nautilus-python/extensions"
    ];
    environment.sessionVariables = {
      NAUTILUS_4_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-4";
      NAUTILUS_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-3.0";
    };
  };
}
