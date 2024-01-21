{
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  home.programs.yazi =
    enabled
    // {
      settings = {
        manager = {
          layout = [0 3 5];
          sort_dir_first = true;
          show_symlink = true;
        };
      };
    };

  user.packages = with pkgs; [
    exiftool # general file info
  ];
}
