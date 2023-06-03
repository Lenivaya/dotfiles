{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.php;
in {
  options.modules.dev.php.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        php
        # phpfactor
        nodePackages.intelephense
      ]
      ++ (with php.packages; [
        composer
      ]);
  };
}
