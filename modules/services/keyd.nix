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
            leftalt = "layer(nav)";
          };

          "nav:A" = {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
          };
        };
        # https://github.com/NixOS/nixpkgs/issues/345167#issuecomment-2380874454
        extraConfig = ''
          [nav+shift]
          j = pagedown
          k = pageup
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
