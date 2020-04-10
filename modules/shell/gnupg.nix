{ config, lib, pkgs, ... }:

{
  my = {
    packages = with pkgs; [ gnupg pinentry ];
    env.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  system.activationScripts.setupGnuPG =
    ''mkdir -p "${config.my.env.GNUPGHOME}" -m 700'';

  systemd.user.services.gpg-agent.serviceConfig.ExecStart = [
    ""
    ''
      ${pkgs.gnupg}/bin/gpg-agent \
           --supervised \
           --allow-emacs-pinentry \
           --default-cache-ttl 1800 \
           --pinentry-program ${pkgs.pinentry}/bin/pinentry-gtk-2
    ''
  ];
}
