{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.gnupg;
in
{
  options.modules.shell.gnupg = with types; {
    enable = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.gnupg.agent = enabled // {
        settings = {
          default-cache-ttl = cfg.cacheTTL;
        };
      };
      user.packages = with pkgs; [ tomb ];
    }

    (mkIf config.this.isHeadful {
      programs.gnupg.agent.pinentryPackage = mkForce pkgs.pinentry-gnome3;
      user.packages = with pkgs; [ seahorse ];
    })
  ]);
}
