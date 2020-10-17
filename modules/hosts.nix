# Blocking some internet shit here

{ config, options, lib, ... }:

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
      hostsPath =
        "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts";
      hostsFile = builtins.fetchurl hostsPath;
    in builtins.readFile "${hostsFile}";
  };
}
