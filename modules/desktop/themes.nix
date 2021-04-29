{ config, lib, pkgs, home-manager, ... }:

with lib.my; {
  home-manager.users.${config.user.name} = {
    gtk = {
      enable = true;
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus";
      };
      theme = {
        package = pkgs.gnome3.gnome_themes_standard;
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
        '';
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
      style = {
        package = pkgs.adwaita-qt;
        name = "adwaita";
      };
    };
  };

  services.xserver.displayManager.sessionCommands = ''
    # GTK2_RC_FILES must be available to the display manager.
    export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
  '';

  home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
}
