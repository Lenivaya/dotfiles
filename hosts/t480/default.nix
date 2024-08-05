# t480 -- thinkpad t480
#
# https://wiki.archlinux.org/title/Lenovo_ThinkPad_T480
# https://github.com/taj-ny/nix-config/blob/c52560a2c6b9ca4d00cff130f99c119c9cf59f69/nixos/thinkpad/throttled.nix
{
  pkgs,
  lib,
  inputs,
  system,
  config,
  ...
}:
with lib;
with lib.my;
{
  imports = [
    ../common.nix
    ./hardware-configuration.nix

    ./phone_cam.nix
    ./picom.nix
    ./power-management.nix
    ./fingerprint/default.nix
  ] ++ (with inputs.nixos-hardware.nixosModules; [ lenovo-thinkpad-t480 ]);

  this.isHeadful = true;

  modules = {
    desktop = enabled // {
      xmonad = enabled;
      isPureWM = true;

      fonts.pragmata = enabled;

      xdg.handlr = enabled;

      apps = {
        rofi = enabled;
        dmenu = enabled;
        dunst = enabled;
        discord = enabled;

        gnome-circle = enabled;
      };

      browsers = {
        default = "google-chrome-unstable";

        firefox = enabled // {
          package = pkgs.firefox_nightly;
          executable = "firefox-nightly";
        };
        chromium = enabled // {
          package = inputs.browser-previews.packages.${pkgs.system}.google-chrome-dev;
        };
        tor = enabled;
      };

      term = {
        alacritty = enabled;
        default = mkForce "alacritty";
      };

      media = {
        spotify = enabled;
        mpv = enabled;

        documents = enabled // {
          pdf = enabled;
          ebook = enabled;
          # latex = enabled;
        };

        graphics = enabled // {
          tools = enabled;
          raster = enabled;
        };

        recording = enabled // {
          video = enabled;
          # audio = enabled;
        };
      };

      # vm = {
      #   qemu = enabled;
      # };
    };

    shell = {
      zsh = enabled;
      tmux = enabled;
      gnupg = enabled;
      direnv = enabled;
      pass = enabled;
      git = enabled;
      weather = enabled;
      yazi = enabled;
    };

    editors = {
      vscode = enabled;
      emacs = enabled // {
        doom = enabled;
        default = true;
      };
      jetbrains = enabled // {
        packages = with pkgs; [ jetbrains-toolbox ]; # KISS
      };
    };

    dev = {
      docker = enabled;
      nix = enabled;
      shell = enabled;
      # elixir = enabled;
      rust = enabled;
      go = enabled;
      # haskell = enabled;
      node = enabled;
      python = enabled;
      # dotnet = enabled // {
      #   dotnetPkgsSdks = with pkgs.dotnetCorePackages; [ sdk_8_0 ];
      # };
      typst = enabled;
    };

    services = {
      ananicy = enabled;
      clipcat = enabled;
      kdeconnect = enabled;
      ssh = enabled;
      keyd = enabled;
      # flatpak = enabled;
      # espanso = enabled;
      tray = enabled // {
        trayApps = [
          "cbatticon"
          "blueman-applet"
          "nm-applet"
          "pasystray"
          "mictray"
          "kdeconnect-indicator"
        ];
      };
    };

    programs = {
      nix-helper = enabled;
      nix-ld = enabled;
    };

    hardware = {
      profiles.laptop = enabled;
      cpu.intel = enabled;
      cpu = {
        # tdp = {
        #   p1.watts = 200;
        #   p1.duration = 28.0;
        #   p2.watts = 29;
        #   p2.duration = 2.44140625e-3;
        # };
        undervolt = enabled // {
          core = -120;
          gpu = -120;
          temp = 97;
        };
      };
      gpu = {
        intel = enabled;
      };
      audio = enabled // {
        effects = enabled;
      };
      fingerprint = enabled;
      touchpad = enabled;
      bluetooth = enabled;
      fs = enabled // {
        ssd = enabled;
      };
    };

    # adblock = enabled;
    zram = enabled;
    bootsplash = enabled;
    fast-networking = enabled;
    powermanagement-resting = enabled // {
      services = [
        "bpftune"
        "docker"
        "fwupd"
        "kdeconnect"
        "picom"
      ];
    };
  };

  nix.package = pkgs.unstable.nixVersions.git;

  services.cpupower-gui = enabled;
  services.hardware.bolt.enable = true;
  services.fwupd = enabled;
  services.psd = enabled // { };

  security.sudo-rs = enabled // {
    extraConfig = ''
      Defaults timestamp_timeout=30
    '';
  };

  nix.gc.automatic = mkForce false;
  programs.nh.clean = enabled // {
    dates = "weekly";
    extraArgs = "--keep-since 1w --keep 3";
  };

  services.clight = {
    # BUG https://github.com/NixOS/nixpkgs/issues/321121
    settings.daytime = {
      sunrise = "07:00";
      sunset = "19:00";
    };

    settings.resumedelay = 5;

    settings.keyboard.disabled = true;
    settings.sensor.devname = "video1"; # because video0 is virtual camera
  };

  boot.kernelPackages =
    let
      kernel' = pkgs.linuxPackages_cachyos-lto;
    in
    mkForce kernel';

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
    # Use TEO as CPUIdle Governor
    "cpuidle.governor=teo"
    "intel_pstate=active"
    # Disabling the HDMI audio output
    "snd_hda_codec_hdmi"
    # Enable powersaving for Intel soundcards
    "snd_hda_intel.power_save=1"
  ];

  networking.firewall = {
    allowedUDPPorts = [
      3000
      4000
      8080
      8000
      1433
      4321
      4322
    ];
    allowedTCPPorts = [
      3000
      4000
      8080
      8000
      1433
      4321
      4322
    ];
  };

  boot.extraModprobeConfig = "
    options thinkpad_acpi experimental=1 fan_control=1
    options psmouse synaptics_intertouch=1
  ";

  user.packages = with pkgs; [
    (inxi.override { withRecommends = true; })
    khal
    telegram-desktop
    ffmpeg-full
    pwvucontrol_git

    video-trimmer

    postman
    my.gitbutler
    scx # user-space schedulers

    protonvpn-gui

    inputs.twitch-hls-client.packages.${pkgs.system}.default

    warp-terminal
  ];

  # services.syncthing = enabled // {
  #   user = config.user.name;
  #   dataDir = "${config.user.home}/Sync";

  #   overrideDevices = true;
  #   overrideFolders = true;
  # };

  hardware.graphics = enabled // {
    extraPackages = with pkgs; [
      libGL
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
  chaotic.mesa-git = enabled // {
    extraPackages = with pkgs; [
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
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = mkForce "iHD";
  };

  services.smartd = enabled;

  # modules.services.zcfan = enabled;
  # services.thermald = mkForce disabled;
  # services.throttled = mkForce enabled;
  services.throttled = mkForce disabled;

  # Dirty hack to have hosts file modifiable
  # (will be discarded on config change or reboot) [1]
  #
  # [1]: https://discourse.nixos.org/t/a-fast-way-for-modifying-etc-hosts-using-networking-extrahosts/4190/3
  environment.etc.hosts.mode = "0644";

  # BPF-based auto-tuning of Linux system parameters
  services.bpftune = enabled;

  # Run appimages seamlesssly
  programs.appimage.binfmt = true;

  services.journald.extraConfig = ''
    SystemMaxUse=50M
    RuntimeMaxUse=10M
    SystemMaxFileSize=50M
  '';

  systemd.coredump.extraConfig = ''
    Storage=none
    ProcessSizeMax=0
  '';

  nix.settings = {
    system-features = [
      "gccarch-x86-64-v3"
      "gccarch-x86-64-v4"
      "big-parallel"
    ];
  };

  home.programs.mpv.config = {
    profile = "fast";
    hwdec = "auto";
  };

  environment.shellAliases = {
    shutUpAndGetOutOfMySight = "sudo modprobe -r uvcvideo && volumectl -m mute";
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
  };

  nixpkgs.overlays =
    let
      optimize = pkg: optimizeForThisHost (withClang pkg);
    in
    [ inputs.nur.overlay ]
    ++ [ inputs.picom.overlay.${system} ]
    ++ [
      (_final: prev: {
        telegram-desktop = prev.telegram-desktop_git;
        alacritty = prev.alacritty_git;
        yt-dlp = prev.yt-dlp_git;
        mpv = prev.mpv-vapoursynth;
        skippy-xd = optimize prev.skippy-xd;
        dmenu = optimize prev.dmenu;
        nsxiv = optimize prev.nsxiv;
        trayer = optimize prev.trayer;
        st = optimize prev.st;
      })
    ];
}
