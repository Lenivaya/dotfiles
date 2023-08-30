# t440p -- thinkpad t440p
# https://github.com/CRAG666/dotfiles/tree/main/thinkpad
{
  pkgs,
  lib,
  inputs,
  config,
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
      # ./jack_retask/jack_retask.nix
      #
      ./postgresql.nix
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

      fonts.pragmata = enabled;
      # fonts.pragmasevka = enabled;

      apps = {
        rofi = enabled;
        dmenu = enabled;
        dunst = enabled;
        discord = enabled;

        gnome-circle = enabled;
      };

      browsers = {
        default = "firefox";

        firefox = enabled;
        chromium = enabled // {googled = true;};
        tor = enabled;
        # qutebrowser = enabled;
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

        recording = enabled // {audio = enabled;};
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
    };

    dev = {
      shell = enabled;
      cc = enabled;
      rust = enabled;
      go = enabled;
      haskell = enabled;
      node = enabled;
      python = enabled;
      dotnet =
        enabled
        // {
          dotnetPkg = pkgs.dotnet-sdk_7;
          dotnetAspNetPkg = pkgs.dotnet-aspnetcore_7;
        };
      # elixir = enabled;
      # php = enabled;
    };

    services = {
      greenclip = enabled;
      kdeconnect = enabled;
      ssh = enabled;
      warp = enabled;
      keyd = enabled;
      flatpak = enabled;
      espanso = enabled;
      # random-wallpaper =
      #   enabled
      #   // {
      #     howOften = "*-*-* 05:00:00"; # daily 5AM
      #   };
      tray =
        enabled
        // {
          trayApps = [
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
          undervolt =
            enabled
            // rec {
              core = -80;
              gpu = -40;
              uncore = core;
              analogio = core;
            };
        };
      gpu.intel = enabled;
      # gpu.nvidia = enabled;
      audio = enabled;
      fingerprint = enabled;
      touchpad = enabled;
      bluetooth = enabled;
      fs = enabled // {ssd = enabled;};
      # zram = enabled;
    };

    adblock = enabled;
    bootsplash = enabled;
  };

  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

  services.fwupd = enabled;

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
    # kernel' = pkgs.unstable.linuxPackages_lqx;
    # kernel' = pkgs.unstable.linuxPackages_zen;
    kernel' = pkgs.unstable.linuxPackages_xanmod_latest;
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

  boot.plymouth = {
    theme = "abstract_ring";
    themePackages = with pkgs.my; [plymouth-themes];
  };

  networking.firewall = {
    allowedUDPPorts = [3000 4000 8080 8000 1433];
    allowedTCPPorts = [3000 4000 8080 8000 1433];
  };

  # For manual fan control with pwm
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

  # nixpkgs.config.permittedInsecurePackages = ["electron-13.6.9"];
  user.packages = with pkgs;
    [
      # lightworks pitivi
      ffmpeg-full
      obsidian
      sqlfluff
    ]
    ++ (with pkgs.unstable.jetbrains; [
      # phpstorm
      webstorm
      rider
      pycharm-professional
    ]);

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

  services.syncthing =
    enabled
    // {
      user = config.user.name;
      dataDir = "${config.user.home}/Sync";

      overrideDevices = true;
      overrideFolders = true;
    };

  services.safeeyes = enabled;

  services.smartd = enabled;

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
    optimize = pkg: optimizeForThisHost (withThinLTO (withClang pkg));
  in [
    (_final: prev: {
      inherit (pkgs.unstable) google-chrome;
      inherit (pkgs.unstable) firefox firefox-bin;
      inherit (pkgs.unstable) vscode vscode-extensions;

      picom = optimize pkgs.unstable.picom-next;
      sxhkd = optimize prev.sxhkd;
      skippy-xd = optimize prev.skippy-xd;
      dmenu = optimize prev.dmenu;
      nsxiv = optimize prev.nsxiv;
      trayer = optimize prev.trayer;

      # TODO FIXME BUG
      # Broken after https://github.com/NixOS/nixpkgs/pull/256413
      linux_lqx_fixed = pkgs.linuxPackagesFor (pkgs.unstable.linuxKernel.kernels.linux_lqx.override {
        structuredExtraConfig = with lib.kernel; {
          DEFAULT_TCP_CONG = freeform "bbr2";
        };
        ignoreConfigErrors = true;
      });
    })
  ];
}
