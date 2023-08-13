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
  cfg = config.modules.services.skippy-xd;
in {
  options.modules.services.skippy-xd.enable = mkBoolOpt true;

  config = mkIf cfg.enable {
    home.configFile."skippy-xd" = {
      source = "${configDir}/skippy-xd";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [skippy-xd];

    systemd.user.services.skippy-xd = mkGraphicalService {
      description = "Windows and workspaces selector";

      path = with pkgs; [skippy-xd];
      script = "skippy-xd --start-daemon";
    };
  };
}
