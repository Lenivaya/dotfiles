{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my;
{
  config = mkIf config.modules.desktop.isPureWM {
    services.clight = enabled // {
      settings = {
        dimmer.disabled = true;
        # gamma.long_transition = true;
      };
    };

    systemd.user.services.clight.path = with pkgs; [ gawk ];
  };
}
