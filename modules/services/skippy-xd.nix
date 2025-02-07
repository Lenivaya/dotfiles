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
  inherit (config.dotfiles) configDir;
  cfg = config.modules.services.skippy-xd;
in
{
  options.modules.services.skippy-xd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."skippy-xd" = {
      source = "${configDir}/skippy-xd";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [ skippy-xd ];

    systemd.user.services.skippy-xd = mkGraphicalService {
      description = "Windows and workspaces selector";

      path = with pkgs; [ skippy-xd ];
      script = "skippy-xd --start-daemon --config --config-reload ${configDir}/skippy-xd/skippy-xd.rc";
    };

    nixpkgs.overlays = [
      (_final: prev: {
        skippy-xd = optimizePkg (
          prev.skippy-xd.overrideAttrs (_oa: {
            src = inputs.skippy-xd;
          })
        );
      })
    ];
  };
}
