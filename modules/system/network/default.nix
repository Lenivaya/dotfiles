{ lib, ... }:
with lib;
with lib.my;
{
  services = {
    # systemd DNS resolver daemon
    resolved = enabled;
  };

  networking = {
    # global dhcp has been deprecated upstream
    # use networkd instead
    # individual interfaces are still managed through dhcp in hardware configurations
    useDHCP = mkDefault false;
    useNetworkd = mkDefault true;

    # dns
    nameservers = [
      # cloudflare, yuck
      # shares data
      "1.1.1.1"
      "1.0.0.1"

      # quad9, said to be the best
      # shares *less* data
      "9.9.9.9"
    ];

    networkmanager = enabled // {
      plugins = [ ]; # disable all plugins, we don't need them
      dns = "systemd-resolved"; # use systemd-resolved as dns backend
      unmanaged = [
        "docker0"
        "rndis0"
      ];
      wifi = {
        macAddress = "random"; # use a random mac address on every boot
        powersave = true; # enable wifi powersaving
      };
    };
  };

  # enable wireless database, it helps with finding the right channels
  hardware.wirelessRegulatoryDatabase = true;

  # allow for the system to boot without waiting for the network interfaces are online
  # speeds up boot times
  systemd = {
    network.wait-online.enable = false;
    services = {
      NetworkManager-wait-online.enable = false;

      # disable networkd and resolved from being restarted on configuration changes
      systemd-networkd.stopIfChanged = false;
      systemd-resolved.stopIfChanged = false;
    };
  };
}
