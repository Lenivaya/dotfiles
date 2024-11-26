{ lib, pkgs, ... }:
with lib;
with lib.my;
let
  postgres' = pkgs.postgresql_17;

  # pgagent = pkgs.stdenv.mkDerivation rec {
  #   name = "pgagent";
  #   nativeBuildInputs = with pkgs; [ cmake ];
  #   buildInputs = with pkgs; [
  #     postgres'
  #     boost
  #   ];

  #   src = pkgs.fetchFromGitHub {
  #     owner = "postgres";
  #     repo = name;
  #     rev = "REL-4_2_2";
  #     sha256 = "sha256-Eoy57VMKtKQd9CyFuVLzimQPlHB4trIg2DLFeBI1w3M=";
  #   };

in
#   installPhase = ''
#     mkdir -p $out/bin
#     cp pgagent $out/bin
#     mkdir -p $out/share/postgresql/extension
#     cp *.{sql,control} $out/share/postgresql/extension
#   '';
# };
{
  user.packages = with pkgs; [
    pgcli
    # pgadmin4-desktopmode
    # pgagent
  ];

  services = {
    postgresql = enabled // {
      enableTCPIP = true;
      package = postgres';
      extraPlugins = with postgres'.pkgs; [
        pg_cron
        hypopg
        pg_repack
        pgroonga # full text search
        pg_uuidv7 # better uuids
        # pgagent
      ];

      settings = {
        shared_preload_libraries = comcat [
          "pg_stat_statements"
          "pg_repack"
        ];
      };

      # This crutch is here because some services cannot work via a UNIX
      # socket connection and I can't be bothered to configure proper
      # authentication.
      authentication = ''
        local all all trust
        host    all        all         all                   md5
        # ipv6
        host all       all     ::1/128        trust
      '';
    };
  };

  user.extraGroups = [ "postgres" ];

  networking.firewall = {
    allowedUDPPorts = [
      5432
    ];
    allowedTCPPorts = [
      5432
    ];
  };
}
