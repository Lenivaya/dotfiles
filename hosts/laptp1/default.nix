# laptp1 -- old laptop used as a server
#
# nixos-rebuild \
#     --flake "$DOTFILES"/.#laptp1 \
#     --show-trace --use-remote-sudo \
#     --impure --fast --no-build-nix \
#     --target-host leniviy@192.168.0.99 \
#     switch
#
{
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
with my;
{
  imports =
    [
      ../common.nix
      ./hardware-configuration.nix
      # ./modules/default.nix
      # ./modules/your-spotify.nix

      inputs.nixos-facter-modules.nixosModules.facter
      { config.facter.reportPath = ./facter.json; }
    ]
    ++ (with inputs.srvos; [
      # nixosModules.server
      # nixosModules.mixins-nginx
    ]);

  # this.isHeadless = true;

  modules = {
    desktop = enabled // {
      xmonad = enabled;
      isPureWM = true;
      fonts.iosevka-full-mono = enabled;
      lockscreen.autoSuspend = false;

      apps = {
        rofi = enabled;
        dmenu = enabled;
        dunst = enabled;
      };
      browsers = {
        default = "firefox";
        firefox = enabled // {
          package = inputs.firefox.packages.${pkgs.system}.firefox-bin;
          executable = "firefox";
        };
      };
      media = {
        mpv = enabled;
      };
    };
    shell = {
      gnupg = enabled;
      tmux = enabled;
      git = enabled;
      zsh = enabled;
    };

    services = {
      ssh = enabled;
      keyd = enabled;
      greenclip = enabled;
      # adguardhome = enabled;
      tray = enabled // {
        trayPkgs = with pkgs; [
          bluez
          networkmanagerapplet
          cbatticon
          gxkb
          pasystray
        ];
        trayApps = [
          "blueman-applet"
          "nm-applet"
          "cbatticon"
          "gxkb"
          "pasystray"
        ];
      };
    };

    hardware = {
      cpu.intel = enabled;
      gpu.intel = enabled;
      fs = enabled;
      touchpad = enabled;
      bluetooth = enabled;
      audio = enabled;
    };

    fast-networking = enabled;
    zram = enabled;
  };

  # services.fwupd = enabled;

  # overriding after srvos
  networking.hostName = mkForce "laptp1";

  # zramSwap = {
  #   algorithm = mkForce "lz4";
  #   memoryPercent = mkForce 50;
  # };

  users.groups.media = { };
  user.extraGroups = [ "media" ];

  services.xserver = {
    videoDrivers = [ "radeon" ];
  };
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "radeonsi";
    VDPAU_DRIVER = "radeonsi";
  };
  hardware.graphics = enabled // {
    extraPackages = with pkgs; [
      mesa.opencl
      libGL
      intel-ocl
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      vpl-gpu-rt
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer
      vulkan-tools
    ];
    extraPackages32 = with pkgs; [
      mesa.opencl
      libGL
      intel-ocl
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      vpl-gpu-rt
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer
      vulkan-tools
    ];
  };

  services.logind.lidSwitchExternalPower = "ignore";
  networking.networkmanager.wifi.powersave = mkForce false;
  networking.networkmanager.wifi.macAddress = mkForce "permanent";
  # services.irqbalance = enabled;
  powerManagement.cpuFreqGovernor = mkForce "performance";
  fileSystems."/".options = [
    "noatime"
    "nodiratime"
  ];

  # Disabling some things that use memory
  # but are completely unrequired
  nix.settings.auto-optimise-store = mkForce false;
  nix.gc.automatic = mkForce false;
  services.thermald = mkForce disabled;
  services.automatic-timezoned = mkForce disabled;
  # location.provider = mkForce "manual";
  # security.polkit = mkForce disabled;
  # hardware.bluetooth = mkForce disabled;
  # fonts.fontconfig = mkForce disabled;
  boot.tmp.useTmpfs = mkForce false;

  boot.kernelPackages =
    let
      # kernel' = pkgs.linuxPackages_cachyos-server;
      kernel' = pkgs.linuxPackages_cachyos-lto;
    in
    mkForce kernel';

  # services.bpftune = enabled;

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
    "nohibernate"
    # https://wiki.archlinux.org/title/improving_performance#Watchdogs
    "nowatchdog"
    "kernel.nmi_watchdog=0"
    "msr.allow_writes=on"
    # Disabling the HDMI audio output
    "snd_hda_codec_hdmi"
    # Enable powersaving for Intel soundcards
    "snd_hda_intel.power_save=1"
    # In some cases, split lock mitigate can slow down performance in some applications and games.
    # A patch is available to disable it via sysctl. [1]
    #
    # [1]: https://wiki.cachyos.org/configuration/general_system_tweaks/
    "kernel.split_lock_mitigate=0"
  ];

  services.scx = enabled // {
    package = pkgs.scx_git.full;
    scheduler = "scx_bpfland";
  };

  # Dirty hack to have hosts file modifiable
  # (will be discarded on config change or reboot) [1]
  #
  # [1]: https://discourse.nixos.org/t/a-fast-way-for-modifying-etc-hosts-using-networking-extrahosts/4190/3
  environment.etc.hosts.mode = "0644";

  services.journald.extraConfig = ''
    SystemMaxUse=50M
    RuntimeMaxUse=10M
    SystemMaxFileSize=50M
  '';

  systemd.coredump.extraConfig = ''
    Storage=none
    ProcessSizeMax=0
  '';

  # https://www.reddit.com/r/Fedora/comments/10s06fd/why_is_systemdoomd_still_a_thing/
  # https://www.reddit.com/r/Ubuntu/comments/uyl4i6/ubuntu_2204s_new_oom_killing_system_is_killing/
  systemd.oomd = disabled;

  nix.settings = {
    system-features = [
      "gccarch-x86-64-v3"
      "gccarch-x86-64-v4"
      "big-parallel"
    ];
  };

  # https://www.reddit.com/r/NixOS/comments/1eqcgom/mitigate_pipewire_webcam_battery_drain/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/2669
  services.pipewire = {
    wireplumber = {
      extraConfig = {
        "10-disable-camera" = {
          "wireplumber.profiles" = {
            main."monitor.libcamera" = "disabled";
          };
        };
      };
    };
  };

  home.programs.mpv.config = {
    profile = "fast";
    hwdec = "auto";
  };

  services.clight = {
    # BUG https://github.com/NixOS/nixpkgs/issues/321121
    settings.daytime = {
      sunrise = "07:00";
      sunset = "18:00";
    };

    settings.resumedelay = 5;

    settings.keyboard.disabled = true;
  };

  environment.shellAliases = {
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
    stopMedia = "sudo systemctl stop radarr sonarr readarr prowlarr calibre-server calibre-web jellyfin";
    startMedia = "sudo systemctl restart radarr sonarr readarr prowlarr calibre-server calibre-web jellyfin";
  };

  environment.systemPackages = with pkgs; [
    (inxi.override { withRecommends = true; })
    rustdesk
    yt-dlp
  ];

  nixpkgs.overlays = [
    (_final: prev: {
      #  inherit (pkgs.unstable) bpftune jellyfin jellyfin-web;
      alacritty = prev.alacritty_git;
      yt-dlp = prev.yt-dlp_git;
      mpv = prev.mpv-vapoursynth;
    })
  ];
}
