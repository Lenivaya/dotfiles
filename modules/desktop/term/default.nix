{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.term;
in
{
  options.modules.desktop.term = {
    default = mkOpt types.str "xterm";
  };

  config = {
    services.xserver.desktopManager.xterm.enable = mkDefault (cfg.default == "xterm");

    environment.sessionVariables.TERM = mkForce cfg.default;
    environment.variables.TERM = mkForce cfg.default;
    env.TERM = mkForce cfg.default;
  };
}
