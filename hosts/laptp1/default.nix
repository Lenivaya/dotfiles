# laptp1 -- my laptop

{ pkgs, lib, ... }: {
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

      media = {
        spotify.enable = true;
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
      rust.enable = true;
      go.enable = true;
      haskell.enable = true;
      node.enable = true;
      python.enable = true;
    };

    services = {
      kdeconnect.enable = true;
      ssh.enable = true;
    };

    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      zram.enable = true;
    };

    hosts.enable = true;
  };

  hardware.cpu.intel.updateMicrocode = true;

  services.xserver = {
    deviceSection = ''
      Option "AccelMethod" "glamor"
      Option "TearFree" "on"
    '';
    useGlamor = true;
  };

  # Fix weird graphical glitches
  # https://github.com/NixOS/nixpkgs/issues/86212#issuecomment-64023258
  # hardware.opengl.package = (import (pkgs.fetchzip {
  #   name = "old-nixpkgs";
  #   url =
  #     "https://github.com/NixOS/nixpkgs/archive/0a11634a29c1c9ffe7bfa08fc234fef2ee978dbb.tar.gz";
  #   sha256 = "0vj5k3djn1wlwabzff1kiiy3vs60qzzqgzjbaiwqxacbvlrci10y";
  # }) { localSystem = "x86_64-linux"; }).mesa.drivers;

  services.xserver.videoDrivers = [ "modesetting" ];
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

  # Optimize power use
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;

  # Latest zen kernel
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_zen;
}
