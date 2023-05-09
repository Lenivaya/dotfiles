{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.espanso;
in {
  options.modules.services.espanso.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.espanso.enable = true;
  };
}
