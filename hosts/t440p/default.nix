# t440p -- thinkpad t440p

{ pkgs, config, lib, inputs, ... }: {
  imports = [
    ../personal.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t440p
  ];

  modules = {
    desktop = {
      xmonad.enable = true;

      # fonts.pragmatapro.enable = true;

      apps = {
        rofi.enable = true;
        dunst.enable = true;
        discord.enable = true;
      };

      browsers = {
        default = "firefox";
        firefox.enable = true;
        chromium = {
          enable = true;
          ungoogled = true;
        };
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
        };
        spotify.enable = true;
        mpv.enable = true;
        # ncmpcpp.enable = true;
      };

      # vm = { qemu.enable = true; };
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
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
    };

    hardware = {
      profiles.laptop.enable = true;
      cpu.intel.enable = true;
      cpu.undervolt = rec {
        # enable = true;
        core = (-100); # 80?
        gpu = (-50);
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
      zram.enable = true;
    };

    hosts.enable = true;

    bootsplash = { enable = true; };
  };

  hardware = {
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      nvidiaPersistenced = lib.mkForce false;
      prime = {
        offload.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:2:0:0";
      };
    };
  };
  environment.systemPackages =
    let
      nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
        export __NV_PRIME_RENDER_OFFLOAD=1
        export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
        export __GLX_VENDOR_LIBRARY_NAME=nvidia
        export __VK_LAYER_NV_optimus=NVIDIA_only
        exec -a "$0" "$@"
      '';
    in
    [ nvidia-offload ];

  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Dpi
  hardware.video.hidpi.enable = true;
  #   services.xserver.dpi = 180;

  services.xserver.libinput.touchpad = {
    naturalScrolling = true;
    accelProfile = "adaptive";
  };

  services.fwupd.enable = true;

  services.tlp.settings.CPU_MAX_PERF_ON_BAT = lib.mkForce 50;

  services.clight.settings.keyboard.disabled = lib.mkForce true;

  # Kernel
  boot.kernelPackages =
    lib.mkForce pkgs.unstable.linuxKernel.packages.linux_lqx;
  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
  ];
}
