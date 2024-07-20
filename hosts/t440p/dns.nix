{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with my; {
  networking.nameservers = mkForce ["127.0.0.1" "::1"];
  networking.networkmanager.dns = mkForce "none";

  systemd.services.dnsproxy = {
    description = "dnsproxy";
    path = with pkgs; [dnsproxy];
    script = spaceConcat [
      "dnsproxy"
      "-l 127.0.0.1"
      "-u quic://$(cat ${config.age.secrets.nextdns.path})"
      "-b 9.9.9.9"
      "-b 1.1.1.1"
      "--cache --cache-optimistic"
      "--cache-size=${toString (megabytesToBytes 50)}"
      "--cache-min-ttl=${toString (hoursToSeconds 10)}"
      "--cache-max-ttl=${toString (hoursToSeconds 96)}"
      "--http3"
    ];
    wantedBy = ["multi-user.target"];
  };
}
