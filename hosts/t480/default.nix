# t480 -- thinkpad t480
#
# https://wiki.archlinux.org/title/Lenovo_ThinkPad_T480
# https://github.com/taj-ny/nix-config/blob/c52560a2c6b9ca4d00cff130f99c119c9cf59f69/nixos/thinkpad/throttled.nix
{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:
with lib;
with my;
{
  imports =
    [
      ../common.nix
      ./hardware-configuration.nix
      ./phone_cam.nix
      ./power-management.nix
      ./fingerprint/default.nix
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [ lenovo-thinkpad-t480 ])
    ++ (with inputs; [
      # determinate.nixosModules.default
      stevenblack-hosts.nixosModule
      {
        networking.stevenBlackHosts = enabled // {
          blockFakenews = true;
          blockGambling = true;
          blockPorn = true;
        };
      }
      resterrs.nixosModules.default
      nixos-facter-modules.nixosModules.facter
      { config.facter.reportPath = ./facter.json; }
    ]);

  this.isHeadful = true;

  modules = {
    desktop = enabled // {
      xmonad = enabled;
      isPureWM = true;

      fonts.pragmata = enabled;
      xdg.handlr = enabled;

      apps = {
        rofi = enabled // {
          optimized = true;
        };
        dmenu = enabled;
        dunst = enabled;
        discord = enabled;
        gnome-circle = enabled;
      };

      browsers = {
        default = "google-chrome-stable";

        firefox = enabled // {
          package = inputs.firefox.packages.${pkgs.system}.firefox-nightly-bin;
          executable = "firefox-nightly";
        };
        chromium =
          let
            chrome' = pkgs.unstable-small.google-chrome;
          in
          enabled
          // {
            package = chrome';
          };
        tor = enabled;
      };

      term = {
        kitty = enabled // {
          singleInstance = true;
          default = true;
        };
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

      vm = {
        winapps = enabled;
        # qemu = enabled;
        # wine = enabled;
      };
    };

    shell = {
      zsh = enabled // {
        # default = true;
      };
      fish = enabled // {
        default = true;
        package = pkgs.unstable.fish;
      };
      tmux = enabled;
      gnupg = enabled;
      direnv = enabled;
      pass = enabled;
      git = enabled;
      weather = enabled;
      yazi = enabled;
    };

    editors = {
      default = mkForce "nvim";
      neovim = enabled;
      vscode = enabled;
      jetbrains = enabled // {
        packages = with pkgs; [ jetbrains-toolbox ]; # KISS
      };
    };

    dev = {
      docker = enabled;
      nix = enabled;
      shell = enabled;
      elixir = enabled;
      rust = enabled;
      go = enabled;
      # haskell = enabled;
      node = enabled // {
        package = pkgs.unstable.nodejs_23;
      };
      python = enabled;
      dotnet = enabled // {
        dotnetPkgsSdks = with pkgs.dotnetCorePackages; [ sdk_9_0 ];
      };
      typst = enabled;
    };

    services = {
      # warp = enabled;
      # ananicy = enabled;
      clipcat = enabled;
      # greenclip = enabled;
      kdeconnect = enabled;
      ssh = enabled;
      keyd = enabled;
      # flatpak = enabled;
      # espanso = enabled;
      tray = enabled // {
        trayPkgs = with pkgs; [
          cbatticon
          blueman
          networkmanagerapplet
          pasystray
          mictray
          udiskie
          plasma5Packages.kdeconnect-kde
          gxkb
          indicator-sound-switcher
        ];
        trayApps = [
          "cbatticon"
          "blueman-applet"
          "nm-applet"
          "pasystray"
          "mictray"
          "kdeconnect-indicator"
          "gxkb"
          "indicator-sound-switcher"
        ];
      };
    };

    programs = {
      nix-helper = enabled;
      nix-ld = enabled;
      distrobox = enabled;
      # gauntlet = enabled;
    };

    cachyos = {
      settings = enabled;
      udev = enabled;
    };

    hardware = {
      profiles.laptop = enabled // {
        autoSuspendOnLowBattery = false;
      };
      ddc = enabled;
      cpu.intel = enabled;
      cpu = {
        # tdp = {
        #   battery = {
        #     risky = true;
        #     p1.watts = 15;
        #     p1.duration = 28.0;
        #     p2.watts = 30;
        #     p2.duration = 2.44140625e-3;
        #     cTDP = 1;
        #   };
        #   ac = {
        #     risky = true;
        #     updateRate = 1;
        #     p1.watts = 64;
        #     p1.duration = 32.0;
        #     p2.watts = 44;
        #     p2.duration = 2.44140625e-3;
        #     cTDP = 2;
        #   };
        # };
        undervolt = enabled // {
          core = -100;
          gpu = -100;
          temp = 100;
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

    zram = enabled;
    bootsplash = enabled;
    fast-networking = enabled;
  };

  nix.package = pkgs.unstable.nixVersions.git;
  # nix.package = pkgs.lix_git;

  services.cpupower-gui = enabled;
  services.hardware.bolt.enable = true;
  services.fwupd = enabled;

  security.sudo = enabled // {
    extraConfig = ''
      Defaults timestamp_timeout=30
    '';
    extraRules =
      let
        mkRule = pkg: cmd: rules: [
          {
            command = "${pkg}/bin/${cmd}";
            options = rules;
          }
          {
            command = "/run/current-system/sw/bin/${cmd}";
            options = rules;
          }
        ];
        mkNoPwd = pkg: cmd: mkRule pkg cmd [ "NOPASSWD" ];
      in
      [
        {
          commands = mkMerge [
            (mkNoPwd pkgs.ps_mem "ps_mem")
            (mkNoPwd pkgs.unixtools.fdisk "fdisk -l")
            (mkNoPwd pkgs.undervolt "undervolt -r")
            (mkNoPwd pkgs.smartmontools "smartctl")
            (mkNoPwd pkgs.powertop "powertop")
            (mkNoPwd pkgs.intel-gpu-tools "intel_gpu_top")
          ];
          groups = [ "wheel" ];
        }
      ];
  };

  nix.gc.automatic = mkForce false;
  programs.nh.clean = enabled // {
    dates = "monthly";
    extraArgs = "--keep-since 1w --keep 3";
  };

  services.clight = {
    # BUG https://github.com/NixOS/nixpkgs/issues/321121
    settings.daytime = {
      sunrise = "07:00";
      sunset = "18:00";
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
    "i915.enable_psr=2"
    # In some cases, split lock mitigate can slow down performance in some applications and games.
    # A patch is available to disable it via sysctl. [1]
    # [1]: https://wiki.cachyos.org/configuration/general_system_tweaks/
    "kernel.split_lock_mitigate=0"
    # Switch to tsc (time stamp counter) at the cost of precision
    "tsc=reliable"
    "clocksource=tsc"

    # https://github.com/MatthewCroughan/nixcfg/blob/afab322e6da20cc038d8577dd4a365673702d183/hosts/t480/configuration.nix
    "i915.modeset=1"
    "i915.fastboot=1"
    "i915.enable_guc=2"
    "i915.enable_psr=1"
    "i915.enable_fbc=1"
    "i915.enable_dc=2"
  ];

  boot.blacklistedKernelModules = [
    "snd_pcsp"
  ];

  # https://github.com/sched-ext/scx
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rustland
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rusty
  # https://www.phoronix.com/news/Rust-Linux-Scheduler-Experiment
  # https://github.com/sched-ext/scx/issues/1188
  # https://wiki.cachyos.org/configuration/sched-ext/#disable-ananicy-cpp
  services.scx = enabled // {
    scheduler = "scx_bpfland";
    package = pkgs.scx_git.full;
    # extraArgs = [
    #   "-p"
    #   "-m performance"
    # ]; # https://github.com/sched-ext/scx/issues/1247
  };

  networking.firewall = {
    allowedUDPPortRanges = [
      {
        from = 3000;
        to = 3007;
      }
      {
        from = 4000;
        to = 4007;
      }
      {
        from = 8000;
        to = 8010;
      }
    ];
    allowedUDPPorts = [
      3000
      4000
      8080
      8000
      1433
      4321
      4322
      24800
    ];
    allowedTCPPorts = [
      3000
      4000
      8080
      8000
      1433
      4321
      4322
      24800
    ];
  };

  boot.extraModprobeConfig = "
    options thinkpad_acpi experimental=1 fan_control=1
    options psmouse synaptics_intertouch=1
  ";

  environment.systemPackages = with pkgs; [
    my.code-cursor
    (inxi.override { withRecommends = true; })
    khal
    ayugram-desktop
    ffmpeg-full
    video-trimmer
    postman
    my.gitbutler
    twitch-hls-client
    curtail # image compression
    smartmontools
    gcc
    obsidian
    qrrs
    iwgtk
    protonvpn-gui
    ungoogled-chromium
    wireguard-tools
    deskflow
    upwork
    beekeeper-studio
    # scx.full
    scx_git.full
    pgcli
    # zed-editor_git
    # zoom-us
    windsurf
    readest

    # dropbox
    maestral-gui
    maestral

    ciscoPacketTracer8
    cozy
  ];

  hardware.trackpoint = enabled // {
    speed = 500;
    sensitivity = 250;
    emulateWheel = true;
  };

  hardware.graphics = enabled // {
    extraPackages = with pkgs; [
      libGL
      intel-ocl
      intel-media-driver
      vaapiIntel
      vaapiVdpauenhanceda
      vpl-gpu-rt
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer
      vulkan-tools
    ];
    extraPackages32 = with pkgs; [
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
  chaotic.mesa-git = enabled // {
    fallbackSpecialisation = false;
    # method = "replaceRuntimeDependencies";
    extraPackages = with pkgs; [
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
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = mkForce "iHD";

    # https://wiki.archlinux.org/title/GTK#GTK_4_applications_are_slow
    GSK_RENDERER = "gl";
    GDK_DEBUG = "gl-no-fractional";
  };

  services.smartd = enabled;

  modules.services.zcfan = enabled;
  # services.thermald = mkForce disabled;
  # services.throttled = mkForce enabled;
  services.throttled = mkForce disabled;

  # Dirty hack to have hosts file modifiable
  # (will be discarded on config change or reboot) [1]
  #
  # [1]: https://discourse.nixos.org/t/a-fast-way-for-modifying-etc-hosts-using-networking-extrahosts/4190/3
  environment.etc.hosts.mode = "0644";

  # BPF-based auto-tuning of Linux system parameters
  # services.bpftune = enabled;

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

  services.avahi = enabled;

  services.resterrs = enabled // {
    settings = {
      system_services_to_stop = [
        "fwupd"
        "syncthing"
        "bpftune"
        "scx"
      ];
      user_services_to_stop = [
        "kdeconnect"
        "easyeffects"
      ];
      apps_to_stop = [
        "spotify"
        "deskflow"
        "maestral"
      ];
      commands_unplugged = [
        "bluetoothctl power off"
      ];
      commands_plugged = [
        "bluetoothctl power on"
      ];
      username = config.user.name;
    };
    extraServicePackages = with pkgs; [
      bluez
    ];
  };

  services.keyd.keyboards.default.ids = mkForce [
    "*"
    # disable touchpad and other things
    "-06cb:0000:11917735" # touchpad
    "-17aa:5054:b7eca923" # buttons
    "-0000:0006:bdb72f48"
    "-05d6:000a:2a5c56c6"
  ];

  networking.wireless.iwd.settings.General.AddressRandomization = "network";
  networking.wireless.iwd.settings.General.AddressRandomizationRange = "full";

  # monitors
  # services.udev.extraRules =
  #   let
  #     bash = "${pkgs.bash}/bin/bash";
  #     ddcciDev = "AUX B/DDI B/PHY B";
  #     ddcciNode = "/sys/bus/i2c/devices/i2c-4/new_device";
  #   in
  #   ''
  #     SUBSYSTEM=="i2c", ACTION=="add", ATTR{name}=="${ddcciDev}", RUN+="${bash} -c 'sleep 30; printf ddcci\ 0x37 > ${ddcciNode}'"
  #   '';

  nixpkgs.overlays =
    [ inputs.nur.overlays.default ]
    ++ [
      (_final: prev: {
        inherit (pkgs.unstable)
          ayugram-desktop
          yazi
          twitch-hls-client
          ungoogled-chromium
          obsidian
          vscode
          jetbrains-toolbox
          clipcat
          windsurf
          readest
          kitty
          neovim
          ;

        # inherit (pkgs.unstable-small)
        #   readest
        #   ;

        # 44.11
        # inherit
        #   (fromRev "c96875e768da1b176250c3bd0edce6b4f7b5d4bb" "sha256-y+Hw+NYmqhAXErmrwF3+DKsXyv0qJuyIs87fnBuNFeI=")
        #   code-cursor
        #   ;

        distrobox = prev.distrobox_git;
        telegram-desktop = prev.telegram-desktop_git;
        yt-dlp = prev.yt-dlp_git;
        mpv = prev.mpv-vapoursynth;
        # neovim = optimizePkg inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
        skippy-xd = optimizePkg prev.skippy-xd;
        dmenu = optimizePkg prev.dmenu;
        nsxiv = optimizePkg prev.nsxiv;
        trayer = optimizePkg prev.trayer;
      })
    ];
}
