# Reconfigures sound card so it won't mute internal
# microphone when headphones are plugged in
{
  config,
  lib,
  pkgs,
  ...
}: let
  soundCard = "/sys/class/sound/hwC1D0";
in {
  # hardware.firmware = [ ( pkgs.writeTextDir "/lib/firmware/hda-jack-retask.fw" ( builtins.readFile ./hda-jack-retask.fw ) ) ];
  systemd.services.jack-retask = {
    wantedBy = ["multi-user.target"];
    before = ["multi-user.target"];

    script = ''
      (
        echo 0x12 0x90a60130
        echo 0x13 0x40000000
        echo 0x14 0x90170110
        echo 0x15 0x0321101f
        echo 0x16 0x21211010
        echo 0x18 0x411111f0
        echo 0x19 0x21a11010
        echo 0x1a 0x40a000f0
        echo 0x1b 0x411111f0
        echo 0x1d 0x40738105
        echo 0x1e 0x411111f0
      )>${soundCard}/user_pin_configs
      echo 1 > ${soundCard}/reconfig
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
    };
  };
}
