# Blocking some internet shit here

{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hosts;
in {
  options.modules.hosts = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    networking.extraHosts = let
      hostsFile = pkgs.fetchFromGitHub {
        owner = "StevenBlack";
        repo = "hosts";
        rev = "0977726aa3fe7e02b72f175816b1a354e86485de";
        sha256 = "0np4pllhclknbnwhx65nxz853gb40zpymk20yd50j22jhwb1jnps";
      };
    in builtins.readFile "${hostsFile}/hosts";
  };
}
