# Simple clipboard manager to be integrated with rofi/dmenu
{
  options,
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.greenclip;
in {
  options.modules.services.greenclip.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.greenclip = enabled;
  };
}
