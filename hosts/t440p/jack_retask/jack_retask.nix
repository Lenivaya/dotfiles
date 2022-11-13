# Reconfigures sound card so it won't mute internal
# microphone when headphones are plugged in
{
  config,
  lib,
  pkgs,
  ...
}: let
  soundCard = "/sys/class/sound/hwC2D0";
in {
  # hardware.firmware = [ ( pkgs.writeTextDir "/lib/firmware/hda-jack-retask.fw" ( builtins.readFile ./hda-jack-retask.fw ) ) ];
  systemd.services.jack-retask = {
    wantedBy = ["multi-user.target"];
    before = ["multi-user.target"];

    script = ''
      (
        echo 0x1a 0x40a000f0
      )>${soundCard}/user_pin_configs
      echo 1 > ${soundCard}/reconfig
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
    };
  };
}
