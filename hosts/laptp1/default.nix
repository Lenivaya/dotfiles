# laptp1 -- my laptop

{ pkgs, options, config, lib, ... }: {
  imports = [ ../common.nix ./hardware-configuration.nix ];

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
      direnv.enable = true;
    };

    editors = {
      emacs.enable = true;
      default = "emacs";
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
    };

    zram.enable = true;
  };

  time.timeZone = "Europe/Kiev";
  location.provider = "geoclue2";

  hardware.cpu.intel.updateMicrocode = true;

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };
  services.dbus.packages = [ pkgs.blueman ];

  services.xserver = {
    deviceSection = ''
      Option "AccelMethod" "glamor"
      Option "TearFree" "on"
    '';
    useGlamor = true;
  };

  # Fix weird graphical glitches
  # https://github.com/NixOS/nixpkgs/issues/86212#issuecomment-64023258
  hardware.opengl.package = (import (pkgs.fetchzip {
    name = "old-nixpkgs";
    url =
      "https://github.com/NixOS/nixpkgs/archive/0a11634a29c1c9ffe7bfa08fc234fef2ee978dbb.tar.gz";
    sha256 = "0vj5k3djn1wlwabzff1kiiy3vs60qzzqgzjbaiwqxacbvlrci10y";
  }) { }).mesa.drivers;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # Optimize power use
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
}
