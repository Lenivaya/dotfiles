{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.zcfan;
in
{
  options.modules.services.zcfan.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

    systemd.services.zcfan = {
      description = "A zero-configuration fan daemon for ThinkPads";
      wantedBy = [ "default.target" ];

      serviceConfig = {
        Restart = "always";
        RestartSec = 0.5;
        TimeoutStopSec = 2;

        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = true;
        RestrictRealtime = true;
      };

      script = getExe pkgs.zcfan;
    };

    environment.systemPackages = with pkgs; [ zcfan ];

    nixpkgs.overlays = [
      (_final: prev: {
        zcfan- = optimizePkg (
          prev.zcfan.overrideAttrs (_oa: {
            src = inputs.zcfan;
          })
        );
      })
    ];
  };
}
