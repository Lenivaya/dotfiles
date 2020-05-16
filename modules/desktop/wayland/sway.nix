{ config, pkgs, options, lib, ... }:

with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {

  options.modules.desktop.sway = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.sway.enable {
    my.home.home.activation.linkSwayConfig =
      dagEntryAfter [ "writeBoundary" ] ''
        [ -d $XDG_CONFIG_HOME/sway ] || ln -sf "$HOME/.dotfiles/config/sway" $XDG_CONFIG_HOME/sway
      '';

    programs.sway = {

      enable = true;
      extraPackages = with pkgs; [
        swayidle
        xwayland # for legacy apps
        waybar # status bar
        mako # notification daemon
        kanshi # autorandr
      ];
    };

    # Here we but a shell script into path, which lets us start sway.service (after importing the environment of the login shell).
    environment.systemPackages = with pkgs;
      [
        (pkgs.writeTextFile {
          name = "startsway";
          destination = "/bin/startsway";
          executable = true;
          text = ''
            #! ${pkgs.bash}/bin/bash

            # first import environment variables from the login manager
            systemctl --user import-environment
            # then start the service
            exec systemctl --user start sway.service
          '';
        })
      ];

    systemd.user.targets.sway-session = {
      description = "Sway compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };

    systemd.user.services.sway = {
      description = "Sway - Wayland window manager";
      documentation = [ "man:sway(5)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
      # We explicitly unset PATH here, as we want it to be set by
      # systemctl --user import-environment in startsway
      environment.PATH = lib.mkForce null;
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
        '';
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    programs.waybar.enable = true;

    systemd.user.services.kanshi = {
      description = "Kanshi output autoconfig ";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        # kanshi doesn't have an option to specifiy config file yet, so it looks
        # at .config/kanshi/config
        ExecStart = ''
          ${pkgs.kanshi}/bin/kanshi
        '';
        RestartSec = 5;
        Restart = "always";
      };
    };
  };

}
