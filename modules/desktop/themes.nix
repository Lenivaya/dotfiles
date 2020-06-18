{ config, lib, pkgs, ... }:

{
  my.home.gtk.enable = true;
  my.home.gtk = {
    iconTheme = {
      package = pkgs.paper-icon-theme;
      name = "Paper";
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

  my.packages = with pkgs; [ adwaita-qt ];
  my.home.xdg.configFile."Trolltech.conf".text = ''
    [Qt]
    style=Adwaita-Dark
  '';

  my.home.home.file.".Xresources".source = <config/xresources/.Xresources>;
}
