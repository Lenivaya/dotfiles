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
    # nixpkgs.overlays = [
    #   (self: super: {
    #     keyd =
    #       optimizeForThisHost
    #       (pkgs.unstable.keyd.overrideAttrs (oa: {
    #         src = builtins.fetchGit {
    #           # Just latest
    #           url = "https://github.com/rvaiya/keyd";
    #           ref = "master";
    #           rev = "41bcceef3c4c1876b5df0a5ba48812fa7c61d908";
    #         };
    #       }));
    #   })
    # ];

    services.keyd = {
      enable = true;
      settings = {
        main = {
          # Better escape (FIXME keyd must be updated in nixpkgs)
          # "j+k" = "esc";
          control = "layer(modified_escape)";
          capslock = "layer(modified_escape)";

          leftalt = "layer(customaltnav)";
        };

        # Ctrl + [ will always trigger escape for all
        # language layouts
        "modified_escape:C" = {
          "[" = "esc";
        };

        "customaltnav:A" = {
          h = "left";
          l = "right";
          k = "up";
          j = "down";
        };
      };
    };

    user.packages = with pkgs; [
      keyd
    ];
  };
}
