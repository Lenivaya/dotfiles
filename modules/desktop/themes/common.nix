{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config.user) name;
in {
  config = mkIf config.modules.desktop.enable {
    xdg.portal.config.common.default = "*";

    home-manager.users.${name} = {
      gtk = let
        gtkSettings = mkMerge [
          {
            gtk-hint-font-metrics = 1;
            gtk-xft-antialias = 1;
            gtk-xft-hinting = 1;
            gtk-xft-hintstyle = "hintslight";
            gtk-xft-rgba = "rgb";
            gtk-application-prefer-dark-theme = true;
          }

          (mkIf config.modules.desktop.isPureWM {
            gtk-cursor-theme-size = 0;

            # Remove min-max-close buttons
            gtk-decoration-layout = "";
            # gtk-decoration-layout = "appmenu:none";
          })
        ];
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
            package = pkgs.gnome.gnome-themes-extra;
            name = "Adwaita";
          };
          gtk3 = mkMerge [
            {
              extraConfig = gtkSettings;
            }

            (mkIf config.modules.desktop.isPureWM {
              extraCss = ''
                * { outline: none; }

                button:focus {
                    outline-style: solid;
                    outline-offset: -2px;
                    outline-width: 1px;
                    -gtk-outline-radius: 2px;
                }
              '';
            })
          ];
          gtk4 = {
            extraConfig = gtkSettings;
          };
        };

      dconf.settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          font-antialiasing = "rgba";
          font-hinting = "hintslight";
        };
      };

      qt =
        enabled
        // {
          # https://pagure.io/fedora-workstation/issue/351
          platformTheme = "gnome";
          style = {
            package = pkgs.adwaita-qt;
            name = "adwaita-dark";
          };
        };
    };

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
      export GTK_THEME=Adwaita-dark
    '';

    home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
  };
}
