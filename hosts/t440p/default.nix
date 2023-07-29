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
  imports = [
    ../common.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t440p
    inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
    ./phone_cam.nix
    ./gpu.nix
    # ./jack_retask/jack_retask.nix
  ];

  modules = {
    desktop = {
      xmonad = enabled;

      fonts.pragmata = enabled;
      # fonts.pragmasevka = enabled;

      apps = {
        rofi = enabled;
        dunst = enabled;
        discord = enabled;

        gnome-circle = enabled;
      };

      browsers = {
        default = "firefox";

        firefox = enabled;
        qutebrowser = enabled;
        chromium =
          enabled // {googled = true;};
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
            raster = enabled;
          };

        recording =
          enabled
          // {
            audio = enabled;
          };
      };

      vm = {
        qemu = enabled;
        # wine = enabled;
      };
    };

    shell = {
      gnupg = enabled;
      direnv = enabled;
      pass = enabled;
      git = enabled;
      zsh = enabled;
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
      # elixir = enabled;
      # csharp = enabled;
      # php = enabled;
    };

    services = {
      kdeconnect = enabled;
      ssh = enabled;
      warp = enabled;
      keyd = enabled;
      flatpak = enabled;
      espanso = enabled;
      tray =
        enabled
        // {
          trayApps = [
            "blueman-applet"
            "nm-applet"
            "mictray"
            "kdeconnect-indicator"
          ];
        };
    };

    hardware = {
      profiles.laptop = enabled;
      touchpad = enabled;
      cpu.intel = enabled;
      cpu.undervolt =
        enabled
        // rec {
          core = -80;
          gpu = -40;
          uncore = core;
          analogio = core;
        };
      gpu.intel = enabled;
      # gpu.nvidia = enabled;
      audio = enabled;
      fingerprint = enabled;
      bluetooth = enabled;
      fs =
        enabled
        // {
          ssd = enabled;
        };
      # zram = enabled;
    };

    adblock = enabled;
    bootsplash = enabled;
  };

  services.fwupd = enabled;

  modules.services.zcfan = enabled;
  # services.thermald = mkForce disabled;
  # services.throttled = mkForce enabled;

  services.tlp.settings.CPU_MAX_PERF_ON_BAT = mkForce 50;
  services.tlp.settings = {
    CPU_SCALING_MIN_FREQ_ON_AC = 0;
    CPU_SCALING_MAX_FREQ_ON_AC = 4000000;
    CPU_SCALING_MIN_FREQ_ON_BAT = 0;
    # CPU_SCALING_MAX_FREQ_ON_BAT = 0;
  };

  services.clight = {
    settings.keyboard.disabled = true;
    settings.sensor.devname = "video1"; # because video0 is virtual camera
  };

  # Kernel
  boot.kernelPackages = let
    kernelPkg = pkgs.unstable.linuxKernel.packages.linux_lqx;
    # kernelPkg = pkgs.unstable.linuxKernel.packages.linux_xanmod_latest;
    # kernelPkg = pkgs.unstable.linuxKernel.packages.linux_xanmod_stable;
  in
    mkForce kernelPkg;

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
  ];

  boot.plymouth = {
    theme = "abstract_ring";
    themePackages = with pkgs.my; [plymouth-themes];
  };

  networking.firewall = {
    allowedUDPPorts = [3000 4000 8080 8000];
    allowedTCPPorts = [3000 4000 8080 8000];
  };

  # For manual fan control with pwm
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";

  # nixpkgs.config.permittedInsecurePackages = ["electron-13.6.9"];
  user.packages = with pkgs;
    [
      # binance
      # lightworks pitivi
      ffmpeg-full
      obsidian
    ]
    ++ (with pkgs.jetbrains; [
      # phpstorm
    ]);

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

  # services.tp-auto-kbbl =
  #   enabled
  #   // {
  #     device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  #   };
  #

  nixpkgs.overlays = [
    (_final: _prev: {
      inherit (pkgs.unstable) google-chrome;

      inherit (pkgs.unstable) firefox firefox-bin;
      inherit (pkgs.unstable) vscode vscode-extensions;
    })
  ];
}
