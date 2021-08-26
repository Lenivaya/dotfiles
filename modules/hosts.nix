# Blocking some internet shit here

{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hosts;
in {
  options.modules.hosts.enable = mkBoolOpt false;

  config = mkIf cfg.enable {

    nixpkgs.overlays = [ inputs.nur.overlay ];

    services.unbound = {
      enable = true;

      settings = {
        server = {
          access-control = [ "127.0.0.0/24 allow" ];

          interface = [ "0.0.0.0" "::" ];

          so-reuseport = true;
          tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";
          tls-upstream = true;

          include = "${pkgs.nur.repos.ambroisie.unbound-zones-adblock}/hosts";
        };

        forward-zone = [{
          name = ".";
          forward-addr = [
            "1.1.1.1@853#cloudflare-dns.com"
            "1.0.0.1@853#cloudflare-dns.com"
          ];
        }];
      };
    };

    # environment.systemPackages = with pkgs;
    #   [ nur.repos.ambroisie.unified-hosts-lists ];

    # networking.extraHosts =
    #   let
    #     hostsFile = pkgs.fetchFromGitHub {
    #       owner = "StevenBlack";
    #       repo = "hosts";
    #       rev = "c202dbda759bb0ab52c68e9f675ccd2ad4b59c3e";
    #       sha256 = "etXlrCUOBU2U/T/lHYOzAtIyLW4k1OXA7Q/+WMvPiZg=";
    #     };
    #   in
    #   builtins.readFile "${hostsFile}/hosts";

  };
}
