{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.dev.ccache;
in
{
  options.modules.dev.ccache.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.ccache.enable = true;
    nix.settings.extra-sandbox-paths = [ config.programs.ccache.cacheDir ];
  };
}
