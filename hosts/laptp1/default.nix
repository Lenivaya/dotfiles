# laptp1 -- my laptop
{
  pkgs,
  lib,
  ...
}: {
  imports = [../personal.nix ./hardware-configuration.nix];

  modules = {
    desktop = {
      xmonad.enable = true;

      fonts.pragmata.enable = true;

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
        st.enable = true;
        alacritty.enable = true;
        default = "st";
      };

      media = {
        documents = {
          enable = true;
          pdf.enable = true;
          ebook.enable = true;
        };
        spotify.enable = true;
        mpv.enable = true;
        ncmpcpp.enable = true;
      };

      vm.qemu.enable = true;
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
      cpu.intel.enable = true;
      fs.enable = true;
      audio.enable = true;
      bluetooth.enable = true;
      zram.enable = true;
    };

    hosts.enable = true;

    bootsplash = {enable = true;};
  };

  zramSwap = {
    algorithm = lib.mkForce "lz4";
    memoryPercent = lib.mkForce 50;
  };

  services.xserver = {
    videoDrivers = ["radeon"];
  };
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

  # Optimize power use
  environment.systemPackages = [pkgs.acpi];
  powerManagement.powertop.enable = true;

  # fancontrol
  # hardware.fancontrol = {
  #   enable = true;
  #   config = ''
  #     INTERVAL=10
  #     DEVPATH=hwmon3=devices/platform/coretemp.0 hwmon4=devices/platform/asus-nb-wmi
  #     DEVNAME=hwmon3=coretemp hwmon4=asus
  #     FCTEMPS=hwmon4/pwm1=hwmon3/temp1_input
  #     FCFANS= hwmon4/pwm1=hwmon4/pwm1
  #     MINTEMP=hwmon4/pwm1=20
  #     MAXTEMP=hwmon4/pwm1=70
  #     MINSTART=hwmon4/pwm1=150
  #     MINSTOP=hwmon4/pwm1=0
  #     MAXPWM=hwmon4/pwm1=160
  #   '';
  # };

  # Kernel
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_lqx;
}
