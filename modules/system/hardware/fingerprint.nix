{
  options,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.fingerprint;
in
{
  options.modules.hardware.fingerprint.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.fprintd = enabled // { };
    security.pam.services.login.fprintAuth = mkDefault true;
    security.pam.services.xscreensaver.fprintAuth = mkDefault true;
  };
}
