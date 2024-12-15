{
  config,
  lib,
  pkgs,
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
            leftalt = "layer(nav)";
          };

          "nav:A" = {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
            "/" = "slash";
          };
          "alternative_left_control_layer" = {
            z = "f1";
            x = "f2";
            c = "f3";
            v = "f4";
            b = "f5";
            n = "f6";
            m = "f7";
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
          [nav+control]
          j = pagedown
          k = pageup

          [nav+shift]
          i = insert
          d = delete

          [nav+control+shift]
          j = end
          k = home
        '';
      };
    };

    environment.systemPackages = with pkgs; [ keyd ];
    users.groups."keyd" = { };
    user.extraGroups = [ "keyd" ];

    systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [
      "CAP_SETGID"
      "CAP_SYS_NICE"
    ];
  };
}
