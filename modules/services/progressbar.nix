# Service to have simple command that will spawn
# graphic progessbars for volume, brightness, etc.
# So "progressbar 80" will spawn 80% filled bar
{
  options,
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
        echo "$1" > "${progressPipe}"
      fi
    '';
  };
in {
  options.modules.services.progressbar = {
    enable = mkBoolOpt true;
    styles = mkOpt types.lines "";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.systemPackages = with pkgs; [progressScript];

      systemd.user.services.progressbar = {
        description = "X progress bar using xob";
        wants = ["graphical-session.target"];
        wantedBy = ["graphical-session.target"];
        after = [
          "graphical-session.target"
          "display-manager.service"
        ];

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
