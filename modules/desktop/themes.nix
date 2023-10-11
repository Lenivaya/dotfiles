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
    gtk = let
      gtkSettings = {
        gtk-cursor-theme-size = 0;
        gtk-application-prefer-dark-theme = true;

        # Remove min-max-close buttons
        gtk-decoration-layout = "";
        # gtk-decoration-layout = "appmenu:none";

        gtk-hint-font-metrics = true;
        gtk-xft-antialias = true;
        gtk-xft-hinting = true;
      };
    in
      enabled
      // {
        theme = {
          package = pkgs.gnome.gnome-themes-extra;
          name = "Adwaita-dark";
        };
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus-Dark";
        };
        cursorTheme = {
          package = pkgs.gnome.gnome-themes-extra;
          name = "Adwaita";
        };
        gtk3 = {
          extraConfig = gtkSettings;
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
          extraConfig = gtkSettings;
        };
      };

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };

    qt =
      enabled
      // {
        # platformTheme = "kde";
        # style = {
        #   name = "breeze";
        # };
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
    export GTK_THEME=Adwaita:dark
  '';

  home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
}
