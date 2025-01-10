# https://news.ycombinator.com/item?id=24344696
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.ddc;
in
{
  options.modules.hardware.ddc = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hardware.i2c.enable = true;
      boot.kernelModules = [
        "ddcci"
        "ddcci_backlight"
      ];
      boot.extraModulePackages = with config.boot.kernelPackages; [ ddcci-driver ];

      environment.systemPackages = with pkgs; [
        ddcutil
      ];

      user.extraGroups = [ "i2c" ];
    }

    (mkIf (config.this.isHeadful) {
      # Also provides 'gddccontrol' GUI
      services.ddccontrol.enable = true;
    })
  ]);
}
