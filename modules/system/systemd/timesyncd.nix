{ config, ... }:
{
  services = {
    # Replace chrony with systemd-timesyncd
    # I am yet to confirm the difference in accuracy
    # but systemd-timesyncd is a part of the systemd
    # ecosystem, which we are already a part of by
    # the virtue of using NixOS.
    chrony.enable = false;
    timesyncd = {
      enable = true;
      servers = config.networking.timeServers; # default value
      # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/systemd/timesyncd.conf.d/timesyncd.conf
      # time.google.com 0.arch.pool.ntp.org 1.arch.pool.ntp.org 2.arch.pool.ntp.org 3.arch.pool.ntp.org
      fallbackServers = [
        "time.cloudflare.com"
        "time.google.com"
        "0.arch.pool.ntp.org"
        "1.arch.pool.ntp.org"
        "2.arch.pool.ntp.org"
        "3.arch.pool.ntp.org"
      ];
      extraConfig = ''
        # poll every 128s as opposed to default 32s
        # should help with battery life a little
        PollIntervalMinSec=128
      '';
    };
  };
}
