{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gnupg.agent =
      enabled
      // {
        # pinentryFlavor = "gnome3";
        pinentryPackage = pkgs.pinentry-gnome3;
      };

    user.packages = with pkgs; [tomb gnome.seahorse];

    # HACK Without this config file you get "No pinentry program" on 20.03.
    #      programs.gnupg.agent.pinentryFlavor doesn't appear to work, and this
    #      is cleaner than overriding the systemd unit.
    home.configFile."gnupg/gpg-agent.conf" = {
      text = ''
        default-cache-ttl ${toString cfg.cacheTTL}
      '';
      # pinentry-program ${getExe pkgs.pinentry-gtk2}
      # pinentry-program ${getExe pkgs.pinentry-gnome3}
    };
  };
}
