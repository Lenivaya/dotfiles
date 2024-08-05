# Simple module that uses udev rules to detect battery unplugging and plugging
# and stop/restart needed services (give them some rest when they are not needed)
#
# https://askubuntu.com/questions/654335/systemd-how-to-start-stop-services-on-battery
#

{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.powermanagement-resting;
in
{
  options.modules.powermanagement-resting = with types; {
    enable = mkBoolOpt false;
    services = mkOpt (listOf str) [ ];
  };

  config = mkIf cfg.enable {
    services.udev.extraRules =
      let
        services = spaceConcat cfg.services;
        systemctl' = getExe' pkgs.systemd "systemctl";
        stop = "${systemctl'} stop ${services} || ${systemctl'} --user stop ${services}";
        start = "${systemctl'} start ${services} || ${systemctl'} --user start ${services}";
      in
      ''
        SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="0", RUN+="${stop}"
        SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="1", RUN+="${start}"
      '';
  };
}
