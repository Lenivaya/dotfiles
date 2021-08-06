# t440p -- thinkpad t440p

{ pkgs, lib, ... }: {
  imports = [ ../personal.nix ./hardware-configuration.nix ];

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
        qutebrowser.enable = true;
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
        # ncmpcpp.enable = true;
      };

      vm = { qemu.enable = true; };
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
      fs = {
        ssd.enable = true;
        enable = true;
      };
      audio.enable = true;
      bluetooth.enable = true;
      zram.enable = true;
    };

    hosts.enable = true;

    bootsplash = { enable = true; };
  };

  services.xserver = { videoDrivers = [ "nvidia" ]; };

  # Optimize power use
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;
  
  # Kernel
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_lqx;
}
