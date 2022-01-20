# Blocking some internet shit here

{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hosts;
in {
  options.modules.hosts.enable = mkBoolOpt false;

  config = mkIf cfg.enable {

    networking.stevenBlackHosts = {
      enable = true;
      blockFakenews = true;
      blockGambling = true;
      # blockPorn = true;
      # blockSocial = true;
    };

    # services.unbound = {
    #   enable = true;

    #   settings = {
    #     server = {
    #       access-control = [ "127.0.0.0/24 allow" ];

    #       interface = [ "0.0.0.0" "::" ];

    #       so-reuseport = true;
    #       tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";
    #       tls-upstream = true;

    #       include = "${pkgs.nur.repos.ambroisie.unbound-zones-adblock}/hosts";
    #     };

    #     forward-zone = [{
    #       name = ".";
    #       forward-addr = [
    #         "1.1.1.1@853#cloudflare-dns.com"
    #         "1.0.0.1@853#cloudflare-dns.com"
    #       ];
    #     }];
    #   };
    # };

  };
}
