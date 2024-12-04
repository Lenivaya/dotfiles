{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.random-wallpaper;
in
{
  options.modules.services.random-wallpaper = with types; {
    enable = mkBoolOpt false;
    howOften = mkOpt str "daily";
    wallpapersDir = mkOpt str "~/Pictures/walls";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (writeScriptBin "random-wallpaper" "systemctl --user restart random-wallpaper.service")
    ];

    systemd.user.timers.random-wallpaper = {
      timerConfig.OnCalendar = cfg.howOften;
      timerConfig.Unit = "random-wallpaper.service";
      wantedBy = [ "timers.target" ];
    };

    systemd.user.services.random-wallpaper = mkGraphicalService {
      serviceConfig.PassEnvironment = "DISPLAY";

      path = with pkgs; [
        feh
        # betterlockscreen
      ];
      script = ''
        feh --randomize --recursive --bg-fill ${cfg.wallpapersDir}
      '';
      # betterlockscreen -u $(
      #   cat ~/.fehbg | grep -o "'[^']*'" | head -n1 | sed "s/'//g"
      # )
    };
  };
}
