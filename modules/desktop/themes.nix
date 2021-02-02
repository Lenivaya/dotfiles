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
        };
        extraCss = ''
          * { outline: none; }
        '';
      };

    };
  };

  # qt5 = {
  #   enable = true;
  #   platformTheme = "gnome";
  #   style = "adwaita";
  # };

  # Try really hard to get QT to respect my GTK theme.
  env.GTK_DATA_PREFIX = [ "${config.system.path}" ];
  env.QT_QPA_PLATFORMTHEME = "gtk2";
  qt5 = {
    style = "gtk2";
    platformTheme = "gtk2";
  };

  services.xserver.displayManager.sessionCommands = ''
    # GTK2_RC_FILES must be available to the display manager.
    export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
  '';

  home.file.".Xresources".source = "${configDir}/xresources/.Xresources";
}
