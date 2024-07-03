{lib, ...}:
with lib;
with my; {
  services.jellyfin =
    enabled
    // {
      openFirewall = true;
      group = "media";
    };

  # For now it doesn't support subpath in reverse proxy [1]
  #
  # [1]: https://github.com/Fallenbagel/jellyseerr/issues/97
  # services.jellyseerr =
  #   enabled
  #   // {
  #     openFirewall = true;
  #     group = "media";
  #   };

  systemd.services.jellyfin.serviceConfig.Nice = mkForce (- 19);
  systemd.services.jellyfin.serviceConfig.IOWeight = mkForce 9000;
  systemd.services.jellyfin.serviceConfig.CPUWeight = mkForce 9000;
}
