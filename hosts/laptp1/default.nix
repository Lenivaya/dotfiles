# laptp1 -- my laptop

{ pkgs, options, config, lib, ... }: {
  imports = [ ../personal.nix ./hardware-configuration.nix ];

  modules = {
    desktop = {
      xmonad.enable = true;

      apps = {
        rofi.enable = true;
        dunst.enable = true;
        zathura.enable = true;
      };

      browsers = {
        default = "firefox";
        firefox.enable = true;
      };

      term = {
        st.enable = true;
        default = "st";
      };
    };

    media = {
      spotify.enable = true;
      mpv.enable = true;
    };

    shell = {
      gnupg.enable = true;
      pass.enable = true;
    };

    editors = {
      emacs.enable = true;
      default = "emacs";
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
    };
  };

  # FIXME No opacity and xrender backend instead of glx (experiencing some problems)
  my.home.services.picom = with lib; {
    inactiveOpacity = mkForce "1.0";
    backend = mkForce "xrender";
  };

  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Kiev";
  location.provider = "geoclue2";

  hardware.cpu.intel.updateMicrocode = true;

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };
  services.dbus.packages = [ pkgs.blueman ];

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.opengl = {
    package = pkgs.unstable.mesa.drivers;
    package32 = pkgs.unstable.pkgsi686Linux.mesa.drivers;
  };

  # I want latest kernel here (why?)
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Optimize power use
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
}
