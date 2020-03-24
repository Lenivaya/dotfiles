# laptp1 -- my laptop

{ pkgs, ... }:
{
  imports = [
    ../personal.nix   # common settings
    ./hardware-configuration.nix
    <modules/zram-swap.nix>

    ## Dekstop environment
    <modules/desktop>
    <modules/desktop/apps>
    <modules/desktop/xmonad.nix>

    <modules/browser/firefox.nix>

    <modules/develop>

    <modules/editors/emacs.nix>

    <modules/services/kdeconnect.nix>
    <modules/services/all.nix>
    <modules/services/ssh.nix>

    <modules/shell/tmux.nix>
    <modules/shell/zsh.nix>
    <modules/shell/git.nix>
    <modules/shell/gnupg.nix>
    <modules/shell/ranger.nix>
  ];

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

  # Optimize power use
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
}
