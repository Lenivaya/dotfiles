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
        rev = "1cd35b6c75db3ac5256a6f724fb91af633418359";
        sha256 = "0al0qlclly4frnsl7yh495876algpz9y1549p52cgbnphfdcj6ij";
      };
    in builtins.readFile "${hostsFile}/hosts";
  };
}
