{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.programs.nix-ld;
in {
  options.modules.programs.nix-ld.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.nix-ld =
      enabled
      // {
        package = pkgs.nix-ld-rs;

        libraries = with pkgs; [
          alsa-lib
          at-spi2-atk
          at-spi2-core
          atk
          cairo
          cups
          curl
          dbus
          expat
          fontconfig
          freetype
          fuse3
          gdk-pixbuf
          glib
          gtk3
          icu
          libGL
          libappindicator-gtk3
          libdrm
          libglvnd
          libnotify
          libpulseaudio
          libunwind
          libusb1
          libuuid
          libxkbcommon
          libxml2
          mesa
          nspr
          nss
          openssl
          pango
          pipewire
          stdenv.cc.cc
          systemd
          vulkan-loader
          xorg.libX11
          xorg.libXScrnSaver
          xorg.libXcomposite
          xorg.libXcursor
          xorg.libXdamage
          xorg.libXext
          xorg.libXfixes
          xorg.libXi
          xorg.libXrandr
          xorg.libXrender
          xorg.libXtst
          xorg.libxcb
          xorg.libxkbfile
          xorg.libxshmfence
          zlib
        ];
      };
  };
}
