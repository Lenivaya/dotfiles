{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) outOfStoreConfigDir;
  cfg = config.modules.services.sxhkd;
in {
  options.modules.services.sxhkd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # home.configFile."sxhkd" = {
    #   source = "${configDir}/sxhkd";
    #   recursive = true;
    # };

    system.userActivationScripts.linkSxhkdrc =
      linkIfNotExist "~/.config/sxhkd" "${outOfStoreConfigDir}/sxhkd";

    user.packages = with pkgs; [sxhkd];
    services.xserver.displayManager.sessionCommands = ''
      ${getExe pkgs.sxhkd} &
    '';

    # FIXME sxhkd in systemd service is luggish
    # systemd.user.services.sxhkd = mkGraphicalService {
    #   description = "Sxhkd hotkeys daemon";

    #   # Not using dash here will lead to noticeable latency[1]
    #   # (at least it is really noticeable for me)
    #   # [1]: https://github.com/baskerville/sxhkd/issues/279
    #   # environment.SXHKD_SHELL = "dash";

    #   # environment.PATH = config.env.PATH;

    #   path = with pkgs; [sxhkd skippy-xd];
    #   script = "sxhkd";
    # };
  };
}
