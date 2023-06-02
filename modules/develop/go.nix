{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.go;
in {
  options.modules.dev.go.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs.go = {
      enable = true;
      package = pkgs.go;
      goBin = "go/bin";
      goPath = "go";
    };

    user.packages = with pkgs; [gotools];
  };
}
