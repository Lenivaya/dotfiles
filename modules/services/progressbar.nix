# Service to have simple command that will spawn
# graphic progessbars for volume, brightness, etc.
# So "progressbar 80" will spawn 80% filled bar
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.progressbar;

  progressPipe = "/tmp/progressbarPipe";
  progressScript = pkgs.writeShellApplication {
    name = "progressbar";
    text = ''
      if [[ $1 =~ ^[+-]?[0-9]+(\.[0-9]+)?!?$ ]]; then
        echo "$1" >> "${progressPipe}"
      fi
    '';
  };
in {
  options.modules.services.progressbar = {
    enable = mkBoolOpt false;
    styles = mkOpt types.lines "";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.systemPackages = [progressScript];

      systemd.user.services.progressbar = mkGraphicalService {
        description = "X progress bar using xob";

        path = with pkgs; [xob];
        script = ''
          rm -f ${progressPipe} && mkfifo ${progressPipe}
          tail -f ${progressPipe} | xob
        '';
      };
    }

    (mkIf (notEmptyString cfg.styles) {
      home.configFile."xob/styles.cfg".text = cfg.styles;
    })
  ]);
}
