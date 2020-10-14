{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = {
    enable = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable {
    my = {
      environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
      user.packages = [ pkgs.tomb ];
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };

      # HACK Without this config file you get "No pinentry program" on 20.03.
      #      program.gnupg.agent.pinentryFlavor doesn't appear to work, and this
      #      is cleaner than overriding the systemd unit.
      home.configFile."gnupg/gpg-agent.conf" = {
        text = ''
          enable-ssh-support
          allow-emacs-pinentry
          default-cache-ttl ${toString gnupgCfg.cacheTTL}
          pinentry-program ${pkgs.pinentry.gtk2}/bin/pinentry
        '';
      };
    };

  };
}
