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
      environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
      programs.gnupg.agent = enabled;
      user.packages = with pkgs; [ tomb ];

      home.configFile."gnupg/gpg-agent.conf" = {
        text = ''
          default-cache-ttl ${toString cfg.cacheTTL}
        '';
        # pinentry-program ${getExe pkgs.pinentry-gnome}
      };
    }

    (mkIf config.this.isHeadful {
      programs.gnupg.agent.pinentryPackage = mkForce pkgs.pinentry-gnome3;
      user.packages = with pkgs; [ seahorse ];
    })
  ]);
}
