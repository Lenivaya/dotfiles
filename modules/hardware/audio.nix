{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    user.extraGroups = [ "audio" ];
    user.packages = with pkgs; [ pulseeffects-pw ];
  };
}
