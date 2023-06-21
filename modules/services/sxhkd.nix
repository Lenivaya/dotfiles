{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.services.sxhkd;
in {
  options.modules.services.sxhkd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."sxhkd" = {
      source = "${configDir}/sxhkd";
      recursive = true;
    };

    systemd.user.services.sxhkd = {
      description = "Sxhkd hotkeys daemon";
      wants = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      after = ["graphical-session.target"];

      path = with pkgs; [sxhkd];
      script = "sxhkd -c ${configDir}/sxhkd/sxhkdrc";
    };
  };
}
