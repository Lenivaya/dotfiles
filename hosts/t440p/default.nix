# t440p -- thinkpad t440p

{ pkgs, lib, inputs, ... }: {
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
        # st.enable = true;
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
      # gpu.intel.enable = true;
      gpu.nvidia.enable = true;
      fs = {
        ssd.enable = true;
        enable = true;
      };
      audio.enable = true;
      bluetooth.enable = true;
      zram.enable = true;
      # nvidia.enable = true;
    };

    hosts.enable = true;

    bootsplash = { enable = true; };
  };

  hardware = {
    nvidia = {
      # nvidiaPersistenced = true;
      # modesetting.enable = true;
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
      exec -a "$0" "$@"
    '';
  in [ nvidia-offload ];

  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Dpi
  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 120;
  fonts.fontconfig.dpi = 120;

  services.xserver.libinput.touchpad = { naturalScrolling = true; };

  boot.kernelParams = [
    "acpi_osi=Linux"
    "acpi_osi='!Windows 2012'"
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
  ];

  # Fingerprint
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.xscreensaver.fprintAuth = true;

  # services.thinkfan = {
  #   enable = true;
  #   levels = [
  #     [ 0 0 49 ]
  #     [ 1 42 65 ]
  #     [ 2 55 68 ]
  #     [ 3 56 71 ]
  #     [ 4 57 73 ]
  #     [ 5 58 75 ]
  #     [ 7 63 32767 ]
  #   ];
  # };
  # boot.extraModprobeConfig = ''
  #   options thinkpad_acpi fan_control=1 experimental=1
  # '';

  services.fwupd.enable = true;

  # Kernel
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_lqx;
}
