{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    sound.enable = true;
    security.rtkit.enable = true;
    user.extraGroups = [ "audio" ];

    # hardware.pulseaudio.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    user.packages = with pkgs;
      [ pulseeffects-pw pulsemixer pamix pulseaudio pavucontrol ]
      ++ [ carla ] # JACK utilities
      ++ [ lsp-plugins rnnoise-plugin ] # Audio plugins
    ;
  };
}
