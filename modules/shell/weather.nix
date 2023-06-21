{
  config,
  options,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.weather;
in {
  options.modules.shell.weather.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [wthrr];

    environment.shellAliases = {
      weather = "wthrr";
      weather-hourly = "wthrr -f d";
      weather-weekly = "wthrr -f w";
    };
  };
}
