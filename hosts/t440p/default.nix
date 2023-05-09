# t440p -- thinkpad t440p
{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
with lib.my; {
  imports = [
    ../personal.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t440p
    inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
    # ./jack_retask/jack_retask.nix
    ./phone_cam.nix
  ];

  modules = {
    desktop = {
      xmonad.enable = true;
      # gnome.enable = true;

      fonts.pragmata.enable = true;

      apps = {
        rofi.enable = true;
        dunst.enable = true;
        discord.enable = true;

        gnome-circle.enable = true;
      };

      browsers = {
        default = "firefox";
        firefox.enable = true;
        chromium = {
          enable = true;
          googled = true;
        };
        qutebrowser.enable = true;
        # brave.enable = true;
      };

      term = {
        alacritty.enable = true;
        default = lib.mkForce "alacritty";
      };

      media = {
        documents = {
          enable = true;
          pdf.enable = true;
          ebook.enable = true;
          latex.enable = true;
        };
        graphics = {
          enable = true;
          raster.enable = true;
        };

        spotify.enable = true;
        mpv.enable = true;
        recording = {
          enable = true;
          audio.enable = true;
        };
      };

      vm = {
        qemu.enable = true;
        wine.enable = true;
      };
    };

    shell = {
      gnupg.enable = true;
      direnv.enable = true;
      pass.enable = true;
      git.enable = true;
      zsh.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        doom.enable = true;
        default = true;
      };
      vscode.enable = true;
    };

    dev = {
      shell.enable = true;
      cc.enable = true;
      rust.enable = true;
      go.enable = true;
      haskell.enable = true;
      node.enable = true;
      python.enable = true;
      elixir.enable = true;
      csharp.enable = true;
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
      warp.enable = true;
      keyd.enable = true;
      espanso.enable = true;
    };

    hardware = {
      profiles.laptop.enable = true;
      touchpad.enable = true;
      cpu.intel.enable = true;
      cpu.undervolt = rec {
        enable = true;
        # Definitely works
        # core = -80;
        # gpu = -40;

        core = -85;
        gpu = -50;
        uncore = core;
        analogio = core;

        temp = 100;
      };
      gpu.intel.enable = true;
      gpu.nvidia.enable = true;
      fs = {
        ssd.enable = true;
        enable = true;
      };
      audio.enable = true;
      fingerprint.enable = true;
      bluetooth.enable = true;
      # zram.enable = true;
    };

    adblock.enable = true;

    bootsplash = {enable = true;};
  };

  # hardware = {
  #   nvidia = {
  #     package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  #     modesetting.enable = true;
  #     powerManagement.enable = lib.mkForce false;
  #     powerManagement.finegrained = lib.mkForce false;
  #     # nvidiaPersistenced = lib.mkForce false;
  #     prime = {
  #       sync.enable = true;
  #       intelBusId = "PCI:0:2:0";
  #       nvidiaBusId = "PCI:2:0:0";
  #     };

  #   };
  # };
  # services.xserver.config = lib.mkAfter ''
  #   Section "OutputClass"
  #       Identifier "nvidia dpi settings"
  #       MatchDriver "nvidia-drm"
  #       Option "UseEdidDpi" "False"
  #       Option "DPI" "96 x 96"
  #   EndSection
  # '';

  hardware = {
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      modesetting.enable = true;
      nvidiaPersistenced = lib.mkForce false;
      powerManagement.enable = lib.mkForce false;
      powerManagement.finegrained = lib.mkForce false;
      # powerManagement.enable = true;
      # powerManagement.finegrained = true;
      # nvidiaPersistenced = lib.mkForce false;
      prime = {
        offload.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:2:0:0";
      };
    };
  };
  environment.systemPackages = let
    nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
      export __NV_PRIME_RENDER_OFFLOAD=1
      export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
      export __GLX_VENDOR_LIBRARY_NAME=nvidia
      export __VK_LAYER_NV_optimus=NVIDIA_only
      exec "$@"
    '';
  in [nvidia-offload];

  # HACK Disable nvidia card for
  # the sake of power consumption
  # hardware.nvidiaOptimus.disable = true;

  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Dpi
  # hardware.video.hidpi.enable = true;
  #   services.xserver.dpi = 180;

  services.fwupd.enable = true;

  services.tlp.settings.CPU_MAX_PERF_ON_BAT = lib.mkForce 50;
  services.tlp.settings = {
    CPU_SCALING_MIN_FREQ_ON_AC = 0;
    CPU_SCALING_MAX_FREQ_ON_AC = 3400000;
    CPU_SCALING_MIN_FREQ_ON_BAT = 0;
    # CPU_SCALING_MAX_FREQ_ON_BAT = 0;
  };

  services.clight = {
    settings.keyboard.disabled = lib.mkForce true;
    settings.sensor.devname = "video1"; # because video0 is virtual camera
  };

  # Kernel
  boot.kernelPackages = let
    kernelPkg = pkgs.unstable.linuxKernel.packages.linux_lqx;
  in
    lib.mkForce kernelPkg;

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

  user.packages = with pkgs; [
    # binance
    ffmpeg-full
    # lightworks pitivi
  ];

  # Flatpak
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  home.file.".local/share/flatpak/overrides/global".text = ''
    [Context]
    filesystems=/run/current-system/sw/share/X11/fonts:ro;/nix/store:ro
  '';

  # Fix for libGL.so error
  hardware.opengl = {
    enable = true;
    setLdLibraryPath = true;
    extraPackages = with pkgs; [libGL];
  };

  # home.services.picom.settings = {
  #   animations = true;
  #   animation-for-open-window = "zoom"; #open window
  #   animation-for-transient-window = "none"; #popup windows
  # };

  nixpkgs.overlays = [
    (self: super: {
      inherit (pkgs.unstable) google-chrome;
      inherit (pkgs.unstable) firefox;

      # Easyeffects + optimized build + fix(?)
      easyeffects =
        optimizeForThisHost
        pkgs.unstable.easyeffects;

      picom =
        optimizeForThisHost
        (pkgs.unstable.picom.overrideAttrs (oa: {
          src = builtins.fetchGit {
            # url = "https://github.com/Arian8j2/picom";
            # ref = "next";

            # url = "https://github.com/dccsillag/picom";
            # ref = "implement-window-animations";

            # Just latest
            url = "https://github.com/yshui/picom";
            ref = "next";
            rev = "cee12875625465292bc11bf09dc8ab117cae75f4";

            # url = "https://github.com/FT-Labs/picom.git";
            # ref = "next";
          };

          # fix for latest next
          buildInputs = oa.buildInputs ++ [pkgs.pcre2];
        }));

      # vscode = pkgs.unstable.vscode;
      # vscode-extensions = pkgs.unstable.vscode-extensions;
    })
  ];
}
