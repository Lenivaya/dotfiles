{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.mpv;
  configDir = config.dotfiles.configDir;
in
{
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = mkIf config.modules.desktop.browsers.firefox.enable
      [ inputs.nur.overlay ];

    home.configFile."mpv" = {
      source = "${configDir}/mpv";
      recursive = true;
    };

    user.packages = with pkgs; [
      mpv-with-scripts
      mpvc # CLI controller for mpv
      (mkIf config.services.xserver.enable celluloid) # nice GTK GUI for mpv
      (mkIf config.modules.desktop.browsers.firefox.enable
        (nur.repos.ambroisie.ff2mpv-go.overrideAttrs (oldAttr: {
          postInstall = ''
            mv $out/bin/ff2mpv-go $out/bin/ff2mpv
            # TODO make manifest generation workable for user
            mkdir -p "$out/lib/mozilla/native-messaging-hosts"
            $out/bin/ff2mpv --manifest > "$out/lib/mozilla/native-messaging-hosts/ff2mpv.json"
          '';

        })))
    ];
  };
}
