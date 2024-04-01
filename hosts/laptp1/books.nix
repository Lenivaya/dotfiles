{lib, ...}:
with lib;
with my; let
  calibre-server-port = 8081;
in {
  networking.firewall.allowedTCPPorts = [calibre-server-port];

  services.readarr =
    enabled
    // {
      openFirewall = true;
      group = "media";
    };

  services.calibre-server =
    enabled
    // {
      port = calibre-server-port;
      group = "media";
      libraries = ["/data/books"];
      auth =
        enabled
        // {
          userDb = "/data/books/users.sqlite";
        };
    };

  services.calibre-web =
    enabled
    // {
      openFirewall = true;
      listen = {
        ip = "127.0.0.1";
        port = 8083;
      };
      group = "media";
      options = {
        enableBookUploading = true;
      };
    };
}
