{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config.user) name;
in {
  home-manager.users.${name} = {
    gtk =
      enabled
      // {
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus";
        };
        theme = {
          package = pkgs.gnome.gnome-themes-extra;
          name = "Adwaita-dark";
        };
        gtk3 = {
          extraConfig = {
            gtk-decoration-layout = "appmenu:none";
            gtk-cursor-theme-name = "Adwaita";
            gtk-cursor-theme-size = 0;
            gtk-application-prefer-dark-theme = true;
          };
          extraCss = ''
            * { outline: none; }

            button:focus {
                outline-style: solid;
                outline-offset: -2px;
                outline-width: 1px;
                outline-radius: 2px;
            }
          '';
        };
        gtk4 = {
          extraConfig = {
            gtk-hint-font-metrics = 1;
            gtk-xft-antialias = 1;
            gtk-xft-hinting = 1;
            gtk-xft-hintstyle = "hintslight";
          };
        };
      };

    qt =
      enabled
      // {
        # TODO add after home-manager update
        # platformTheme = "kde";
        # style = {
        #   name = "breeze";
        # };
        platformTheme = "gnome";
        style = {
          package = pkgs.adwaita-qt;
          name = "adwaita-dark";
        };
      };

    # https://pagure.io/fedora-workstation/issue/351
    # qt =
    #   enabled
    #   // {
    #     platformTheme = "gnome";
    #     style = {
    #       package = pkgs.adwaita-qt;
    #       name = "adwaita-dark";
    #     };
    #   };
  };

  services.xserver.displayManager.sessionCommands = ''
    # GTK2_RC_FILES must be available to the display manager.
    export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
    export GTK_THEME=Adwaita:dark
  '';

  home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
}
