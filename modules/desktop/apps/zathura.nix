{ config, lib, pkgs, ...}:

{
  my.packages = with pkgs; [
    zathura
  ];

  my.home.xdg.configFile."zathura" = {
    source = <config/zathura>;
    recursive = true;
  };
}
