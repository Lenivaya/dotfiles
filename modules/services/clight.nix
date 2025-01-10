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
        verbose = false;
        dimmer.disabled = true;
        # gamma.long_transition = true;
      };
    };

    systemd.user.services.clight.path = with pkgs; [ gawk ];

    # Hate those libddcutil logs
    systemd.services.clightd.serviceConfig.StandardOutput = mkForce "null";
    systemd.services.clightd.serviceConfig.StandardError = mkForce "null";
    systemd.services.clightd.serviceConfig.LogLevelMax = mkForce 2; # error
  };
}
