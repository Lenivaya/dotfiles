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
with my;
{
  imports = [
    ../common.nix
    ./hardware-configuration.nix

    ./phone_cam.nix
    ./power-management.nix
    ./fingerprint/default.nix

    inputs.resterrs.nixosModules.default

    inputs.nixos-facter-modules.nixosModules.facter
    { config.facter.reportPath = ./facter.json; }

    inputs.stevenblack-hosts.nixosModule
    {
      networking.stevenBlackHosts = enabled // {
        blockFakenews = true;
        blockGambling = true;
      };
    }
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
        default = "firefox";

        firefox = enabled // {
          package = inputs.firefox.packages.${pkgs.system}.firefox-bin;
          executable = "firefox";
        };
        chromium =
          let
            chrome' = inputs.browser-previews.packages.${pkgs.system}.google-chrome;
          in
          enabled
          // {
            package = chrome';
          };
        tor = enabled;
      };

      term = {
        kitty = enabled;
        default = mkForce "kitty";
      };

      media = {
        spotify = enabled;
        mpv = enabled;

        documents = enabled // {
          pdf = enabled;
          ebook = enabled;
          latex = enabled;
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
      elixir = enabled;
      rust = enabled;
      go = enabled;
      # haskell = enabled;
      node = enabled;
      python = enabled;
      dotnet = enabled // {
        dotnetPkgsSdks = with pkgs.dotnetCorePackages; [ sdk_9_0 ];
      };
      typst = enabled;
    };

    services = {
      # warp = enabled;
      # ananicy = enabled;
      # clipcat = enabled;
      greenclip = enabled;
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
        undervolt = enabled // rec {
          core = -110;
          gpu = -110;
          # core = -50;
          # gpu = -50;
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

  # nix.package = pkgs.unstable.nixVersions.git;
  # nix.package = pkgs.lix_git;

  services.cpupower-gui = enabled;
  services.hardware.bolt.enable = true;
  services.fwupd = enabled;
  # services.psd = enabled // { };

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
    #
    # [1]: https://wiki.cachyos.org/configuration/general_system_tweaks/
    "kernel.split_lock_mitigate=0"
  ];

  # https://github.com/sched-ext/scx
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rustland
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rusty
  # https://www.phoronix.com/news/Rust-Linux-Scheduler-Experiment
  services.scx = enabled // {
    # package = pkgs.scx_git.full;
    scheduler = "scx_bpfland";
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
    code-cursor
    (inxi.override { withRecommends = true; })
    khal
    telegram-desktop
    # inputs.ayugram-desktop.packages.${pkgs.system}.ayugram-desktop
    ffmpeg-full
    video-trimmer
    postman
    my.gitbutler
    my.twitch-hls-client
    # warp-terminal
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
    neovide
    beekeeper-studio
    # zoom-us
  ];

  hardware.graphics = enabled // {
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
  # chaotic.mesa-git = enabled // {
  #   fallbackSpecialisation = false;
  #   # method = "replaceRuntimeDependencies";
  #   extraPackages = with pkgs; [
  #     libGL
  #     intel-ocl
  #     intel-media-driver
  #     vaapiIntel
  #     vaapiVdpau
  #     vpl-gpu-rt
  #     vulkan-loader
  #     vulkan-validation-layers
  #     vulkan-extension-layer
  #     vulkan-tools
  #   ];
  # };
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
  services.bpftune = enabled;

  # Run appimages seamlesssly
  programs.appimage.binfmt = true;

  services.journald.extraConfig = ''
    SystemMaxUse=50M
    RuntimeMaxUse=10M
    SystemMaxFileSize=50M
  '';

  # systemd.coredump.extraConfig = ''
  #   Storage=none
  #   ProcessSizeMax=0
  # '';

  # https://www.reddit.com/r/Fedora/comments/10s06fd/why_is_systemdoomd_still_a_thing/
  # https://www.reddit.com/r/Ubuntu/comments/uyl4i6/ubuntu_2204s_new_oom_killing_system_is_killing/
  # systemd.oomd = disabled;

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

  environment.shellAliases = {
    shutUpAndGetOutOfMySight = "sudo modprobe -r uvcvideo && volumectl -m mute";
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
  };

  services.avahi = enabled;

  # services.safeeyes = enabled;

  services.resterrs = enabled // {
    settings = {
      system_services_to_stop = [
        "fwupd"
        "syncthing"
        "bpftune"
      ];
      user_services_to_stop = [
        "kdeconnect"
        "picom"
        "easyeffects"
      ];
      apps_to_stop = [
        # "telegram-desktop"
        # "vesktop"
        "deskflow"
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

  # networking.wireless.iwd.settings.General.AddressRandomization = "network";
  # networking.wireless.iwd.settings.General.AddressRandomizationRange = "full";

  home.programs.emacs.package = pkgs.emacs30;

  # services.xserver.displayManager.lightdm = mkForce disabled;
  # services.displayManager.ly = enabled // { };

  # monitors
  services.udev.extraRules =
    let
      bash = "${pkgs.bash}/bin/bash";
      ddcciDev = "AUX B/DDI B/PHY B";
      ddcciNode = "/sys/bus/i2c/devices/i2c-4/new_device";
    in
    ''
      SUBSYSTEM=="i2c", ACTION=="add", ATTR{name}=="${ddcciDev}", RUN+="${bash} -c 'sleep 30; printf ddcci\ 0x37 > ${ddcciNode}'"
    '';

  nixpkgs.overlays =
    let
      optimize = pkg: optimizeForThisHost (withClang pkg);
    in
    [ inputs.nur.overlays.default ]
    ++ [ inputs.picom.overlay.${system} ]
    ++ [
      (_final: prev: {
        inherit (pkgs.unstable)
          code-cursor
          ;

        distrobox = prev.distrobox_git;
        # telegram-desktop = prev.telegram-desktop_git;
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
