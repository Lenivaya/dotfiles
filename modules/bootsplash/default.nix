{
  config,
  options,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.bootsplash;
in {
  options.modules.bootsplash = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    boot.plymouth = {
      enable = true;
    };
  };
}
