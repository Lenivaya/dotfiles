# laptp1 -- my laptop
{
  pkgs,
  lib,
  ...
}: {
  imports = [../common.nix ./hardware-configuration.nix];

  modules = {
    desktop = {
      xmonad.enable = true;
      isPureWM = true;

      apps = {
        rofi.enable = true;
        dunst.enable = true;
      };

      browsers = {
        default = "google-chrome-stable";
        chromium = {
          enable = true;
          ungoogled = true;
        };
      };

      term = {
        st.enable = true;
        default = "st";
      };

      media = {
        documents = {
          enable = true;
          pdf.enable = true;
          ebook.enable = true;
        };
        mpv.enable = true;
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
    };

    dev = {
      shell.enable = true;
      cc.enable = true;
      # rust.enable = true;
      # go.enable = true;
      # haskell.enable = true;
      # node.enable = true;
      # python.enable = true;
      # elixir.enable = true;
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
    };

    hardware = {
      profiles.laptop = enabled;
      cpu.intel.enable = true;
      fs.enable = true;
      audio.enable = true;
      bluetooth.enable = true;
      zram.enable = true;
    };

    adblock.enable = true;

    bootsplash = {enable = true;};
  };

  zramSwap = {
    algorithm = lib.mkForce "lz4";
    memoryPercent = lib.mkForce 50;
  };

  services.xserver = {videoDrivers = ["radeon"];};
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

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
