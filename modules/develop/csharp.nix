{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.csharp;
in {
  options.modules.dev.csharp.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      mono
      #unity unityhub
      omnisharp-roslyn
    ];
  };
}
