# t440p -- thinkpad t440p
# https://github.com/CRAG666/dotfiles/tree/main/thinkpad
{
  pkgs,
  lib,
  inputs,
  system,
  ...
}:
with lib;
with lib.my; {
  imports =
    [
      ../common.nix
      ./hardware-configuration.nix
      ./phone_cam.nix
      ./gpu.nix
      ./postgresql.nix
      # ./jack_retask/jack_retask.nix
    ]
    ++ (with inputs.nixos-hardware.nixosModules; [
      lenovo-thinkpad-t440p
      common-pc-laptop-acpi_call
      # common-pc-laptop-ssd
      # common-pc-laptop-hdd
    ]);

  modules = {
    desktop = {
      xmonad = enabled;
      isPureWM = true;

      fonts.pragmata = enabled;

      apps = {
        rofi = enabled;
        dmenu = enabled;
        dunst = enabled;
        discord = enabled;

        gnome-circle = enabled;
      };

      browsers = {
        default = "google-chrome-stable";

        chromium = enabled // {googled = true;};
        tor = enabled;
      };

      term = {
        alacritty = enabled;
        default = mkForce "alacritty";
      };

      media = {
        spotify = enabled;
        mpv = enabled;

        documents =
          enabled
          // {
            pdf = enabled;
            ebook = enabled;
            latex = enabled;
          };

        graphics =
          enabled
          // {
            tools = enabled;
            raster = enabled;
          };

        recording =
          enabled
          // {
            # audio = enabled;
            video = enabled;
          };
      };

      vm = {
        qemu = enabled;
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
    };

    editors = {
      vscode = enabled;
      emacs =
        enabled
        // {
          doom = enabled;
          default = true;
        };
      jetbrains =
        enabled
        // {
          packages = with pkgs.nur.repos.tehplague.jetbrains; [
            webstorm
            rider
            pycharm-professional
          ];
        };
    };

    dev = {
      shell = enabled;
      # cc = enabled;
      # elixir = enabled;
      rust = enabled;
      go = enabled;
      haskell = enabled;
      node = enabled;
      python = enabled;
      dotnet =
        enabled
        // {
          dotnetPkgsSdks = let
            eol_dotnet =
              (
                import inputs.nixpkgs_eol_dotnet {
                  inherit system;
                }
              )
              .dotnetCorePackages
              .sdk_3_1;
          in
            with pkgs.dotnetCorePackages;
              [
                sdk_8_0
                sdk_6_0
              ]
              ++ [
                eol_dotnet
              ];
        };
      php =
        enabled
        // {
          package = pkgs.php83;
        };
    };

    services = {
      greenclip = enabled;
      kdeconnect = enabled;
      ssh = enabled;
      warp = enabled;
      keyd = enabled;
      flatpak = enabled;
      # espanso = enabled;
      # random-wallpaper =
      #   enabled
      #   // {
      #     howOften = "*-*-* 05:00:00"; # daily 5AM
      #   };
      tray =
        enabled
        // {
          trayApps = [
            "cbatticon"
            "blueman-applet"
            "nm-applet"
            "mictray"
          ];
        };
    };

    hardware = {
      profiles.laptop = enabled;
      cpu.intel =
        enabled
        // {
          # undervolt =
          #   enabled
          #   // rec {
          #     core = -80;
          #     gpu = -40;
          #     uncore = core;
          #     analogio = core;
          #   };
        };
      gpu = {
        intel = enabled;
        # nvidia = enabled;
      };
      audio = enabled // {effects = true;};
      fingerprint = enabled;
      touchpad = enabled;
      bluetooth = enabled;
      fs = enabled // {ssd = enabled;};
      # zram = enabled;
    };

    adblock = enabled;
    bootsplash = enabled;
  };

  services.tlp.settings.CPU_MAX_PERF_ON_BAT = mkForce 50;
  services.tlp.settings = {
    # work at maximum on AC
    # CPU_SCALING_GOVERNOR_ON_AC = mkForce "performance";
    CPU_ENERGY_PERF_POLICY_ON_AC = mkForce "performance";
    CPU_SCALING_MAX_FREQ_ON_AC = MHz 4000;
  };

  services.clight = {
    settings.keyboard.disabled = true;
    settings.sensor.devname = "video1"; # because video0 is virtual camera
  };

  boot.kernelPackages = let
    kernel' = pkgs.linuxPackages_zen;
  in
    mkForce kernel';

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"

    # https://wiki.archlinux.org/title/improving_performance#Watchdogs
    "nowatchdog"
    "kernel.nmi_watchdog=0"
  ];

  boot.plymouth = rec {
    theme = "abstract_ring";
    themePackages = with pkgs; let
      theme' = adi1090x-plymouth-themes.override {selected_themes = [theme];};
    in [theme'];
  };

  networking.firewall = {
    allowedUDPPorts = [3000 4000 8080 8000 1433];
    allowedTCPPorts = [3000 4000 8080 8000 1433];
  };

  # For manual fan control with pwm
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

  user.packages = with pkgs; [
    # lightworks pitivi
    ffmpeg-full
    sqlfluff
    kdenlive
  ];

  hardware.trackpoint = {
    enable = true;
    speed = 500;
    sensitivity = 250;
  };

  powerManagement = let
    modprobe = "${pkgs.kmod}/bin/modprobe";
  in
    enabled
    // {
      # This fixes an issue with not being able to suspend or wake up from
      # suspend due to a kernel bug[1] which is still not fixed.
      #
      # I guess this can also be fixed differently[2], which does look a lot nicer
      # but I just can't bother.
      #
      # [1]: https://bbs.archlinux.org/viewtopic.php?id=270964
      # [1]: https://bugs.launchpad.net/ubuntu/+source/linux/+bug/522998
      # [1]: https://bugs.launchpad.net/ubuntu/+source/pm-utils/+bug/562484/comments/3
      # [1]: https://gist.github.com/ioggstream/8f380d398aef989ac455b93b92d42048
      # [2]: https://linrunner.de/tlp/settings/runtimepm.html
      powerDownCommands = "${modprobe} -r xhci_pci";
      powerUpCommands = "${modprobe} xhci_pci";
    };

  # Fix for libGL.so error
  hardware.opengl =
    enabled
    // {
      setLdLibraryPath = true;
      extraPackages = with pkgs; [libGL];
    };

  # services.syncthing =
  #   enabled
  #   // {
  #     user = config.user.name;
  #     dataDir = "${config.user.home}/Sync";

  #     overrideDevices = true;
  #     overrideFolders = true;
  #   };

  services.safeeyes = enabled;

  services.smartd = enabled;

  # modules.services.zcfan = enabled;
  # services.thermald = mkForce disabled;
  # services.throttled = mkForce enabled;

  # home.services.picom.vSync = mkForce false;
  # home.services.picom.backend = mkForce "xrender";
  # home.services.picom.settings = let
  #   animationExclude = [
  #     "class_g *= 'xmobar'"
  #     "class_g *= 'xmonad'"
  #     "class_g *= 'xmonad-prompt'"
  #     "name *= 'xmobar'"
  #     "name *= 'xmonad'"
  #     "name *= 'xmonad-prompt'"
  #     "class_g *= 'slop'"
  #     "name *= 'slop'"
  #     "class_g *= 'skippy-xd'"
  #     "class_g *= 'skippy-xd'"
  #     "class_g *= 'safeeyes'"
  #   ];
  # in {
  #   corner-radius = 0;

  #   animations = false;
  #   # animation-stiffness = 100;
  #   # animation-window-mass = 0.8;
  #   # animation-dampening = 10;
  #   # animation-clamping = true;

  #   animation-open-exclude = animationExclude;
  #   animation-unmap-exclude = animationExclude;

  #   inactive-exclude = [
  #     "window_type = 'dock'"
  #     "window_type = 'desktop'"
  #     "window_type = 'menu'"
  #     "window_type = 'popup_menu'"
  #     "window_type = 'dropdown_menu'"
  #     "window_type = 'toolbar'"
  #     "window_type = 'notification'"
  #     "class_g *= 'avizo-service'"
  #     "class_g *= 'slop'"
  #     "name *= 'slop'"
  #   ];
  # };

  # services.tp-auto-kbbl =
  #   enabled
  #   // {
  #     device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  #   };

  environment.shellAliases = {
    shutUpAndGetOutOfMySight = "sudo modprobe -r uvcvideo && volumectl -m mute";
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
  };

  nixpkgs.overlays = let
    optimize = pkg: optimizeForThisHost (withClang pkg);
  in
    [inputs.nur.overlay]
    ++ [inputs.picom.overlay.${system}]
    ++ [
      (_final: prev: {
        inherit
          (pkgs.unstable)
          firefox
          firefox-bin
          vscode
          vscode-extensions
          telegram-desktop
          ;

        # dotnetCorePackages =
        #   prev.dotnetCorePackages
        #   // {
        #     sdk_3_1_EOL = prev.dotnetCorePackages.sdk_6_0.overrideAttrs (oa: {
        #       eol = false;
        #       src = oldAttrs.overrideAttrs (oldSrcAttrs: {
        #         sha256 = "https://download.visualstudio.microsoft.com/download/pr/e89c4f00-5cbb-4810-897d-f5300165ee60/027ace0fdcfb834ae0a13469f0b1a4c8/dotnet-sdk-3.1.426-linux-x64.tar.gz";
        #         url = "https://download.visualstudio.microsoft.com/download/pr/e89c4f00-5cbb-4810-897d-f5300165ee60/027ace0fdcfb834ae0a13469f0b1a4c8/dotnet-sdk-3.1.426-linux-x64.tar.gz";
        #       });
        #     });
        #   };

        skippy-xd = optimize prev.skippy-xd;
        dmenu = optimize prev.dmenu;
        nsxiv = optimize prev.nsxiv;
        trayer = optimize prev.trayer;
        st = optimize prev.st;
      })
    ];
}
