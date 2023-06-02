{
  options,
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.fingerprint;
in {
  options.modules.hardware.fingerprint.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.fprintd.enable = true;
    security.pam.services.login.fprintAuth = true;
    security.pam.services.xscreensaver.fprintAuth = true;
  };
}
