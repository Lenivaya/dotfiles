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
      ./modules/default.nix
      ./power-management.nix
      ./postgresql.nix
      # ./jack_retask/jack_retask.nix
      inputs.nixos-facter-modules.nixosModules.facter
      { config.facter.reportPath = ./facter.json; }
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [
      lenovo-thinkpad-t440p
    ])
    ++ (with inputs.srvos; [ nixosModules.mixins-nginx ]);

  this.isHeadful = true;

  modules = {
    desktop = enabled // {
      xmonad = enabled;
      isPureWM = true;
      lockscreen.autoSuspend = false;

      fonts.adwaita-pragmata = enabled;

      xdg.handlr = enabled;

      apps = {
        rofi = enabled;
        dmenu = enabled;
        dunst = enabled;
        discord = enabled;

        gnome-circle = enabled;
      };

      browsers = {
        default = "google-chrome-stable";

        # firefox = enabled // {
        #   package = inputs.firefox.packages.${pkgs.system}.firefox-bin;
        #   executable = "firefox";
        # };
        chromium =
          let
            chrome' = inputs.browser-previews.packages.${pkgs.system}.google-chrome;
          in
          enabled
          // {
            package = chrome';
          };
      };

      term = {
        kitty = enabled;
        default = mkForce "kitty";
      };

      media = {
        # spotify = enabled;
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
          # video = enabled;
        };
      };

      vm = {
        qemu = enabled;
        # virtualbox = enabled;
        # wine = enabled;
      };
    };

    shell = {
      # zsh = enabled;
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
      # vscode = enabled;
      # emacs = enabled // {
      #   doom = enabled;
      #   default = true;
      # };
      neovim = enabled;
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
      # ananicy = enabled;
      clipcat = enabled;
      # greenclip = enabled;
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

    cachyos = {
      settings = enabled;
      udev = enabled;
    };

    hardware = {
      profiles.laptop = enabled;
      cpu.intel = enabled;

      cpu = {
        tdp = {
          battery = {
            risky = true;
            p1.watts = 37;
            p1.duration = 28.0;
            p2.watts = 47;
            p2.duration = 2.44140625e-3;
            cTDP = 1;
          };
          ac = {
            risky = true;
            updateRate = 1;
            p1.watts = 80;
            p1.duration = 28.0;
            p2.watts = 80;
            p2.duration = 2.44140625e-3;
            cTDP = 2;
          };
        };
        undervolt = enabled // rec {
          core = -70;
          gpu = -30;
          uncore = core;
          analogio = core;
          temp = 100;
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

    zram = enabled;
    bootsplash = enabled;
    # fast-networking = enabled;
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

  services.scx = enabled // {
    scheduler = "scx_bpfland";
    package = pkgs.scx_git.full;
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
    # In some cases, split lock mitigate can slow down performance in some applications and games.
    # A patch is available to disable it via sysctl. [1]
    #
    # [1]: https://wiki.cachyos.org/configuration/general_system_tweaks/
    "kernel.split_lock_mitigate=0"
    # Switch to tsc (time stamp counter) at the cost of precision
    "tsc=reliable"
    "clocksource=tsc"
  ];

  # boot.plymouth = rec {
  #   theme = "abstract_ring";
  #   themePackages = with pkgs; let
  #     theme' = adi1090x-plymouth-themes.override {selected_themes = [theme];};
  #   in [theme'];
  # };

  networking.firewall = {
    allowedTCPPortRanges = [
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
      6379
      6969
      1234
    ];
  };

  # For manual fan control with pwm
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

  environment.systemPackages = with pkgs; [
    khal
    ffmpeg-full

    ayugram-desktop
    # video-trimmer
    # kdenlive
    # lightworks pitivi
    # teams-for-linux
    # postman
    my.gitbutler
    protonvpn-gui
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

    # https://wiki.archlinux.org/title/GTK#GTK_4_applications_are_slow
    GSK_RENDERER = "gl";
    GDK_DEBUG = "gl-no-fractional";
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
  # services.thermald = mkForce disabled;
  # services.throttled = mkForce enabled;
  services.throttled = mkForce disabled;

  # services.tp-auto-kbbl =
  #   enabled
  #   // {
  #     device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  #   };

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

  nixpkgs.overlays =
    let
      optimize = pkg: optimizeForThisHost (withClang pkg);
    in
    [ inputs.nur.overlays.default ]
    # ++ [ inputs.picom.overlay.${system} ]
    ++ [
      (_final: prev: {
        inherit (pkgs.unstable)
          ayugram-desktop
          kitty
          neovim
          clipcat
          adguardhome
          ;

        # inherit (pkgs.unstable-small)
        #   scx
        #   ;
        #
        intel-vaapi-driver = prev.intel-vaapi-driver.override { enableHybridCodec = true; };

        telegram-desktop = prev.telegram-desktop_git;
        yt-dlp = prev.yt-dlp_git;

        picom = optimize prev.picom;
        skippy-xd = optimize prev.skippy-xd;
        dmenu = optimize prev.dmenu;
        nsxiv = optimize prev.nsxiv;
        trayer = optimize prev.trayer;
        st = optimize prev.st;
      })
    ];
}
