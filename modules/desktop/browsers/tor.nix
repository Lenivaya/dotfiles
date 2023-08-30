{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers.tor;
in {
  options.modules.desktop.browsers.tor.enable = mkBoolOpt false;

  config = mkIf cfg.enable (
    {
      user.packages = with pkgs; let
        # Let the daemon start automatically if it's stopped
        tor-browser' = writeShellApplication {
          name = "tor-browser";
          text = ''
            systemctl is-active --quiet tor || systemctl start tor
            ${getExe tor-browser-bundle-bin}
          '';
        };
      in [
        tor-browser'

        (makeDesktopItem {
          name = "tor-browser";
          desktopName = "Start tor browser";
          icon = "tor-browser";
          exec = getExe tor-browser';
          categories = ["Network" "WebBrowser" "Security"];
        })
      ];

      services.tor =
        enabled
        // {
          client = enabled;
          # transparentProxy = enabled; route all traffic through tor
        };

      # Smooth scrolling
      environment.sessionVariables.MOZ_USE_XINPUT2 = "1";
    }
    # Let it be disabled on system start up
    # and only star with browser opening
    // disableService "tor"
  );
}
