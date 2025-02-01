{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (config.user) name;
in
{
  config = mkIf config.modules.desktop.enable {
    xdg.portal.config.common.default = "*";
    programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];

    # GTK3 plugin to show popup to search compatible application's menus
    programs.plotinus.enable = mkDefault true;

    home-manager.users.${name} = {
      gtk =
        let
          gtkSettings = mkMerge [
            {
              #   gtk-hint-font-metrics = false;
              gtk-hint-font-metrics = 1;
              gtk-xft-antialias = 1;
              gtk-xft-hinting = 1;
              gtk-xft-hintstyle = "hintslight";
              gtk-xft-rgba = "rgb";
              gtk-enable-animations = false;
              gtk-application-prefer-dark-theme = true;
              gtk-overlay-scrolling = false;
              gtk-enable-input-feedback-sounds = 0;
              gtk-enable-event-sounds = 0;
            }

            (mkIf config.modules.desktop.isPureWM {
              gtk-cursor-theme-size = 0;

              # Remove min-max-close buttons
              gtk-decoration-layout = "";
              # gtk-decoration-layout = "appmenu:none";
            })
          ];
          extraCss = readFile "${configDir}/gtk/css/gtk.css";
        in
        enabled
        // {
          theme = {
            # Theme parser error: gtk.css:5:1-133: Failed to import: Error opening file /nix/store/w2av0y3h2gf7hryc5ib1acivbi5pvjdg-gnome-themes-extra-3.28/share/themes/Adwaita-dark/gtk-4.0/gtk.css: No such file or directory
            # package = pkgs.gnome.gnome-themes-extra;
            # name = "Adwaita-dark";
            package = pkgs.adw-gtk3;
            name = "adw-gtk3-dark";
          };
          iconTheme = {
            package = pkgs.papirus-icon-theme;
            name = "Papirus-Dark";
            # package = pkgs.morewaita-icon-theme;
            # name = "MoreWaita";
          };
          cursorTheme = {
            package = pkgs.gnome-themes-extra;
            name = "Adwaita";
          };
          gtk3 = mkMerge [
            {
              extraConfig = gtkSettings;
            }

            (mkIf config.modules.desktop.isPureWM {
              inherit extraCss;
            })
          ];
          gtk4 = mkMerge [
            {
              extraConfig = gtkSettings;
            }

            (mkIf config.modules.desktop.isPureWM {
              inherit extraCss;
            })
          ];
        };

      dconf.settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          font-antialiasing = "rgba";
          font-hinting = "hintslight";
          enable-animations = false;
        };
      };

      qt = enabled // {
        # , systemsettings
        # platformTheme.name = "kde";
        # style = {
        #   name = "Breeze Dark";
        #   package = pkgs.kdePackages.breeze;
        # };
        #
        # https://pagure.io/fedora-workstation/issue/351
        platformTheme.name = "adwaita";
        style = {
          # package = pkgs.adwaita-qt;
          name = "adwaita-dark";
        };
      };
    };

    qt = enabled // {
      style = "adwaita-dark";
      platformTheme = "gnome";
    };

    environment.systemPackages = with pkgs; [
      # Libraries and programs to ensure
      # that QT applications load without issues, e.g. missing libs.
      libsForQt5.qt5.qtwayland # qt5
      kdePackages.qtwayland # qt6
      qt6.qtwayland
      kdePackages.qqc2-desktop-style

      # qt5ct/qt6ct for configuring QT apps imperatively
      libsForQt5.qt5ct
      kdePackages.qt6ct

      # Some KDE applications such as Dolphin try to fall back to Breeze
      # theme icons. Lets make sure they're also found.
      libsForQt5.breeze-qt5
      kdePackages.breeze-icons
      qt6.qtsvg # needed to load breeze icons

      # Libraries to ensure that "gtk" platform theme works
      # as intended after the following PR:
      # <https://github.com/nix-community/home-manager/pull/5156>
      libsForQt5.qtstyleplugins
      qt6Packages.qt6gtk2
    ];
    environment.sessionVariables = {
      # Scaling factor for QT applications. 1 means no scaling
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      QT_ENABLE_HIGHDPI_SCALING = "1";

      # Do remain backwards compatible with QT5 if possible.
      DISABLE_QT5_COMPAT = "0";
      # Tell Calibre to use the dark theme, because the
      # default light theme hurts my eyes.
      CALIBRE_USE_DARK_PALETTE = "1";
    };

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
      export GTK_THEME=Adwaita-dark
    '';

    home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
  };
}
