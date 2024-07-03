{
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.ssh.startAgent = true;

    services.openssh =
      enabled
      // {
        settings.X11Forwarding = mkForce true;
        settings.PermitRootLogin = "no";
        settings.PasswordAuthentication = mkForce true;
        startWhenNeeded = true;

        # Allow local LAN to connect with passwords
        extraConfig = ''
          Match address 192.168.0.0/24
          PasswordAuthentication yes
        '';
      };

    user.openssh.authorizedKeys.keys =
      if config.user.name == "leniviy"
      then [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKJWZChgyuytRTYKH/TkMY0rS7QTUXzdlSfa0SRSrqK/ xocada@gmail.com"
      ]
      else [];
  };
}
