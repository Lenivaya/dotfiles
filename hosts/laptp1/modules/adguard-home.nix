{lib, ...}:
with lib;
with lib.my; let
  adguardPort = 3000;
in {
  services.resolved = mkForce disabled;

  networking = {
    firewall = {
      allowedTCPPorts = [adguardPort];
      allowedUDPPorts = [53];
    };
  };

  services.adguardhome =
    enabled
    // {
      mutableSettings = true;
      openFirewall = true;
      port = adguardPort;

      settings = {
        dns = {
          enable_dnssec = true;
          upstream_mode = "parallel";
          upstream_dns = [
            "quic://dns.nextdns.io"
            "https://dns.quad9.net/dns-query"
            "https://doh.libredns.gr/dns-query"
            "https://doh.dns.sb/dns-query"
            "https://freedns.controld.com/uncensored"
          ];
          bootstrap_dns = [
            "9.9.9.9"
            "1.1.1.1"
          ];

          cache_optimistic = true;
          cache_size = megabytesToBytes 50;
          cache_ttl_min = hoursToSeconds 1;
          cache_ttl_max = hoursToSeconds 24;
        };
      };
    };

  systemd.services.adguardhome.serviceConfig.Nice = mkForce (- 20);
  systemd.services.adguardhome.serviceConfig.IOWeight = mkForce 10000;
  systemd.services.adguardhome.serviceConfig.CPUWeight = mkForce 10000;
}
