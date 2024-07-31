{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config) modules;
  cfg = config.modules.desktop.apps.dmenu;
in
{
  options.modules.desktop.apps.dmenu.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages =
      with pkgs;
      [
        emojipick

        # (writeShellScriptBin "dmenu-drun"
        #   "${getExe j4-dmenu-desktop} --no-generic --dmenu='dmenu -g 2 -l 10'")
        (writeShellScriptBin "dmenu-network" "${getExe networkmanager_dmenu} -l 10")
        (writeShellScriptBin "dmenu-bluetooth" "${getExe dmenu-bluetooth} -p 'bluetooth' -g 2 -l 5")
        (writeShellScriptBin "dmenu-translate" "${getExe my.dmenu-translate} -p 'trans:' -l 10")
        (writeShellScriptBin "dmenu-audio" "${getExe my.dmenu-pipewire} -- -p 'sinks:' -l 10")
        # (writeShellScriptBin "dmenu_udiskie"
        #   "${getExe my.udiskie-dmenu} -p 'devices' -l 10")
      ]
      ++ optional modules.services.greenclip.enable (
        writeShellScriptBin "dmenu-greenclip" "greenclip print | grep . | dmenu -l 10 -g 3 -l 5 -p clipboard | xargs -r -d'\n' -I '{}' greenclip print '{}'"
      );
  };
}
