{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.keyd;
in {
  options.modules.services.keyd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # Fixes needed to support latest keyd version (TODO push this to nixpkgs?)
    nixpkgs.overlays = [
      (_final: prev: {
        keyd = withThinLTO (withClang (
          optimizeForThisHost (prev.keyd.overrideAttrs (_oa: {
            src = prev.fetchFromGitHub {
              owner = "rvaiya";
              repo = "keyd";
              rev = "2338f11b1ddd81eaddd957de720a3b4279222da0";
              hash = "sha256-IR0Murvc9aCSuGh01sZEWHyYmmFADtmopGMgt6ydSxU=";
            };

            makeFlags = [
              "DESTDIR=${placeholder "out"}"
              "PREFIX="
              "SOCKET_PATH=/run/keyd/keyd.sock"
            ];
          }))
        ));
      })
    ];
    systemd.services.keyd.serviceConfig = {
      PrivateUsers = mkForce false;
      CapabilityBoundingSet = mkForce ["CAP_SYS_NICE"];
      SystemCallFilter =
        mkForce ["nice" "@system-service" "~@privileged" "setpriority"];
    };

    services.keyd =
      enabled
      // {
        # settings = {
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
          "modified_escape:C" = {"[" = "esc";};

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

    user.packages = with pkgs; [keyd];
    user.extraGroups = ["keyd"];
  };
}
