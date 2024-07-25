# https://github.com/musnix/musnix ?
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;
    effects = {
      enable = mkBoolOpt false;
      morePlugins = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      sound = enabled;
      security.rtkit = enabled;
      user.extraGroups = [
        "audio"
        "pipewire"
      ];

      boot.kernelModules =
        [
          "snd_seq"
          "snd_seq_midi"
          "snd_rawmidi"
        ] # ALSA Sequencer kernel modules
        ++ [
          "snd_pcm_oss"
          "snd_mixer_oss"
          "snd_seq_oss"
        ]
        ++ [ "uinput" ]; # AVRCP protocol support/compatibility for input device

      services.pipewire = enabled // {
        alsa = enabled // {
          support32Bit = with pkgs; (stdenv.hostPlatform.isLinux && stdenv.hostPlatform.isx86);
        };
        pulse = enabled;
        jack = enabled;
      };

      user.packages =
        with pkgs;
        [
          pulsemixer
          pamix
          pamixer
          pulseaudio
        ]
        ++ optionals config.modules.desktop.enable [
          helvum
          pavucontrol
        ];
    }

    (mkIf cfg.effects.enable {
      # home.services.easyeffects = enabled;
      user.packages = with pkgs; [ easyeffects ];
    })

    # (mkIf
    #   (cfg.effects.enable && cfg.effects.morePlugins)
    #   {
    #     # user.packages = with pkgs;
    #     #   [carla] # JACK utilities
    #     #   ++ [lsp-plugins dragonfly-reverb rnnoise-plugin] # Audio plugins
    #     #   ++ [
    #     #     distrho
    #     #     swh_lv2
    #     #     calf
    #     #     # ir.lv2
    #     #   ];

    #     # environment.variables = with lib;
    #     #   listToAttrs (map (type:
    #     #     nameValuePair "${toUpper type}_PATH" [
    #     #       "$HOME/.${type}"
    #     #       "$HOME/.nix-profile/lib/${type}"
    #     #       "/run/current-system/sw/lib/${type}"
    #     #     ]) ["dssi" "ladspa" "lv2" "lxvst" "vst" "vst3"]);
    #   })
  ]);
}
