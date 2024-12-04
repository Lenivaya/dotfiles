# t440p -- thinkpad t440p
# https://github.com/CRAG666/dotfiles/tree/main/thinkpad
#
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
  imports =
    [
      ../common.nix
      ./hardware-configuration.nix
      ./phone_cam.nix
      ./gpu.nix
      ./picom.nix
      # ./dns.nix
      ./modules/default.nix
      ./power-management.nix
      ./postgresql.nix
      # ./mongodb.nix
      # ./jack_retask/jack_retask.nix
      inputs.nixos-facter-modules.nixosModules.facter
      { config.facter.reportPath = ./facter.json; }
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [
      lenovo-thinkpad-t440p
      common-pc-laptop-acpi_call
      # common-pc-laptop-ssd
      # common-pc-laptop-hdd
    ])
    ++ (with inputs.srvos; [ nixosModules.mixins-nginx ]);

  this.isHeadful = true;

  modules = {
    desktop = enabled // {
      xmonad = enabled;
      isPureWM = true;
      lockscreen.autoSuspend = false;

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
        default = "firefox-nightly";

        firefox = enabled // {
          package = inputs.firefox.packages.${pkgs.system}.firefox-bin;
          executable = "firefox-nightly";
        };
        # chromium =
        #   let
        #     chrome' = inputs.browser-previews.packages.${pkgs.system}.google-chrome;
        #   in
        #   enabled
        #   // {
        #     package = chrome';
        #   };
        # tor = enabled;
        # qutebrowser = enabled;
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
          # tools = enabled;
          # raster = enabled;
        };

        recording = enabled // {
          # audio = enabled;
          video = enabled;
        };
      };

      vm = {
        qemu = enabled;
        # virtualbox = enabled;
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
      # vscode = enabled;
      emacs = enabled // {
        doom = enabled;
        default = true;
      };
      # jetbrains = enabled // {
      #   packages = with pkgs; [ jetbrains-toolbox ]; # KISS
      # };
    };

    dev = {
      docker = enabled;
      nix = enabled;
      shell = enabled;
      # cc = enabled;
      # elixir = enabled;
      rust = enabled;
      go = enabled;
      # haskell = enabled;
      node = enabled;
      python = enabled;
      # dotnet = enabled // {
      #   dotnetPkgsSdks = with pkgs.dotnetCorePackages; [ sdk_8_0 ];
      # };

      # typst = enabled;
    };

    services = {
      adguardhome = enabled;
      ananicy = enabled;
      # clipcat = enabled;
      greenclip = enabled;
      kdeconnect = enabled;
      ssh = enabled;
      # warp = enabled;
      keyd = enabled;
      # flatpak = enabled;
      # espanso = enabled;
      # random-wallpaper =
      #   enabled
      #   // {
      #     howOften = "*-*-* 05:00:00"; # daily 5AM
      #   };
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
        tdp = {
          p1.watts = 37;
          p1.duration = 28.0;
          p2.watts = 47;
          p2.duration = 2.44140625e-3;
        };
        undervolt = enabled // rec {
          core = -70;
          gpu = -30;
          temp = 95;
          uncore = core;
          analogio = core;
        };
      };
      gpu = {
        intel = enabled;
        # nvidia = enabled;
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
  };

  # nix.package = pkgs.unstable.nixVersions.git;

  services.fwupd = enabled;
  services.cpupower-gui = enabled;

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
          commands =
            (mkNoPwd pkgs.ps_mem "ps_mem")
            ++ (mkNoPwd pkgs.unixtools.fdisk "fdisk -l")
            ++ (mkNoPwd pkgs.undervolt "undervolt -r")
            ++ (mkNoPwd pkgs.smartmontools "smartctl");
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

    settings.keyboard.disabled = true;
    settings.sensor.devname = "video1"; # because video0 is virtual camera
  };

  boot.kernelPackages =
    let
      kernel' = pkgs.linuxPackages_cachyos-lto;
    in
    mkForce kernel';

  # https://github.com/sched-ext/scx
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rustland
  # https://github.com/sched-ext/scx/tree/main/scheds/rust/scx_rusty
  # https://www.phoronix.com/news/Rust-Linux-Scheduler-Experiment
  services.scx = enabled // {
    # package = pkgs.unstable.scx.rustscheds;
    package = pkgs.scx_git.full;
    scheduler = "scx_bpfland";
  };

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
    # Enables RC6, RC6p and RC6pp.
    # Last two are only available on Sandy Bridge CPUs (circa 2011).
    "i915.enable_rc6=7"
    "intel_pstate=active"
    # Disabling the HDMI audio output
    "snd_hda_codec_hdmi"
    # Enable powersaving for Intel soundcards
    "snd_hda_intel.power_save=1"
  ];

  # boot.plymouth = rec {
  #   theme = "abstract_ring";
  #   themePackages = with pkgs; let
  #     theme' = adi1090x-plymouth-themes.override {selected_themes = [theme];};
  #   in [theme'];
  # };

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
      24800
    ];
  };

  # For manual fan control with pwm
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

  environment.systemPackages = with pkgs; [
    khal
    telegram-desktop
    # inputs.ayugram-desktop.packages.${pkgs.system}.ayugram-desktop
    ffmpeg-full
    # video-trimmer
    # kdenlive
    # lightworks pitivi
    # teams-for-linux
    # postman
    my.gitbutler
    protonvpn-gui
    # inputs.twitch-hls-client.packages.${pkgs.system}.default
    smartmontools
    # my.devtunnel
    # warp-terminal
    # zed-editor_git
    deskflow
  ];

  # hardware.trackpoint = {
  #   enable = true;
  #   speed = 500;
  #   sensitivity = 250;
  # };

  hardware.graphics = enabled // {
    extraPackages = with pkgs; [
      libGL
      intel-ocl
      vpl-gpu-rt
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer
      vulkan-tools
    ];
  };
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = mkForce "i965";
  };

  services.smartd = enabled;

  # services.syncthing = enabled // {
  #   user = config.user.name;
  #   dataDir = "${config.user.home}/Sync";

  #   overrideDevices = true;
  #   overrideFolders = true;
  # };

  # services.safeeyes = enabled;

  # modules.services.zcfan = enabled;
  services.thermald = mkForce disabled;
  services.throttled = mkForce enabled;

  # services.tp-auto-kbbl =
  #   enabled
  #   // {
  #     device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  #   };

  environment.shellAliases = {
    shutUpAndGetOutOfMySight = "sudo modprobe -r uvcvideo && volumectl -m mute";
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
  };

  # home.programs.emacs.package = mkForce pkgs.emacs29;

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
  };

  # Mostly use it as second screen with home-server capabilities, if I want to
  # put it to sleep I'll do it manually
  services.logind.lidSwitchExternalPower = "ignore";

  services.avahi = enabled;

  services.dnsmasq = mkForce disabled;

  nixpkgs.overlays =
    let
      optimize = pkg: optimizeForThisHost (withClang pkg);
    in
    [ inputs.nur.overlay ]
    ++ [ inputs.picom.overlay.${system} ]
    ++ [
      (_final: prev: {
        intel-vaapi-driver = prev.intel-vaapi-driver.override { enableHybridCodec = true; };
        # btop = prev.btop.override {
        #   cudaSupport = true;
        # };

        telegram-desktop = prev.telegram-desktop_git;
        alacritty = prev.alacritty_git;
        yt-dlp = prev.yt-dlp_git;
        mpv = prev.mpv-vapoursynth;

        picom = optimize prev.picom;
        skippy-xd = optimize prev.skippy-xd;
        dmenu = optimize prev.dmenu;
        nsxiv = optimize prev.nsxiv;
        trayer = optimize prev.trayer;
        st = optimize prev.st;
      })
    ];
}
