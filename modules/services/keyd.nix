{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.keyd;
in
{
  options.modules.services.keyd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.keyd = enabled // {
      keyboards.default = {
        settings = {
          main = {
            "j+k" = "esc";
            "d+f" = "delete";

            capslock = "overload(control, esc)";
            leftcontrol = "overload(alternative_left_control_layer, leftcontrol)";
            leftalt = "layer(alt-layer)";

            # almost never used it as shift, but is a lot easier than moving finger to backspace
            rightshift = "backspace";

            leftshift = "oneshot(shift)";
            # leftmeta = "oneshot(meta)";
            rightalt = "oneshot(alt)";
          };

          "alt-layer:A" = {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
            p = "C-v";
            y = "C-c";
            u = "C-z";
            v = "C-a"; # let's assume v means V
            x = "C-w"; # easier to close browser tabs
            "/" = "C-f";
            w = "C-right";
            b = "C-left";
          };
          "alternative_left_control_layer" = {
            z = "f1";
            x = "f2";
            c = "f3";
            v = "f4";
            b = "f5";
            m = "f7";
            n = "f6";
            comma = "f8";
            period = "f9";
            slash = "f10";
            space = "rightmouse";

            # to work across different keyboard layouts
            "i" = "i";
            "o" = "o";
            "u" = "u";
            "a" = "a";
            "/" = "slash";
          };
        };
        # https://github.com/NixOS/nixpkgs/issues/345167#issuecomment-2380874454
        extraConfig = ''
          [alt-layer+control]
          j = pagedown
          k = pageup

          [alt-layer+shift]
          i = insert
          d = delete

          [alt-layer+control+shift]
          j = end
          k = home
        '';
      };
    };

    environment.systemPackages = with pkgs; [ keyd ];
    # users.groups."keyd" = { };
    # user.extraGroups = [ "keyd" ];
    #
    # systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [
    #   "CAP_SETGID"
    #   "CAP_SYS_NICE"
    # ];
    #
    nixpkgs.overlays = [
      (_final: prev: {
        keyd = optimizePkg (
          # prev.keyd
          prev.keyd.overrideAttrs (_oa: {
            src = inputs.keyd;
          })
        );
      })
    ];
  };
}
