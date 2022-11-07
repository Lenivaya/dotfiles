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
    gtk = {
      enable = true;
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
              outline-width: 2px;
              outline-radius: 2px;
          }
        '';
      };
    };

    qt = {
      enable = true;
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
