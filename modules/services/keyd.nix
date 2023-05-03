{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.keyd;
in {
  options.modules.services.keyd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.keyd = {
      enable = true;
      settings = {
        main = {
          control = "oneshot(control)";
          capslock = "overload(esc, control)";
        };
      };
    };

    user.packages = with pkgs; [
      keyd
    ];
  };
}
# [ids]
# *
# [main]
# shift = oneshot(shift)
# meta = oneshot(meta)
# control = oneshot(control)
# capslock = overload(esc, control)
