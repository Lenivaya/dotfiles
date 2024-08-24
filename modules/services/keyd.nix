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
      keyboards.default.settings = {
        main = {
          # Better escape
          "j+k" = "esc";

          control = "layer(modified_escape)";
          capslock = "layer(modified_escape)";

          # nav layer
          leftalt = "layer(nav)";
        };

        # Ctrl + [ will always trigger escape for all
        # language layouts
        "modified_escape:C" = {
          "[" = "esc";
        };

        "nav:A" = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
        };

        # that is broken by nixos module (TODO)
        # "nav+shift" = {
        #   j = "pagedown";
        #   k = "pageup";
        # };
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
