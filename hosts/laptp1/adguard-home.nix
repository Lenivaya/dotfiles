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
      settings = {
        bind_port = adguardPort;
        schema_version = 20;
        dns = {
          enable_dnssec = true;
          upstream_dns = [
            "https://dns.quad9.net/dns-query"
            "https://dns.nextdns.io"
            "https://dns.google/dns-query"
            "https://dns.cloudflare.com/dns-query"
          ];
          bootstrap_dns = [
            "1.1.1.1"
            "9.9.9.9"
          ];

          cache_optimistic = true;
          cache_size = 10000000;
          cache_ttl_min = 3600;
          cache_ttl_max = 86400;
        };
      };
    };

  systemd.services.adguardhome.serviceConfig.Nice = mkForce (- 20);
}
