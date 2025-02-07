{
  lib,
  config,
  inputs,
  system,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.compositor;
  inherit (config.dotfiles) configDir;
in
{
  options.modules.desktop.compositor.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.services.picom = enabled;
    home.configFile."picom/picom.conf".source = mkForce "${configDir}/picom/picom.conf";

    nixpkgs.overlays = [
      (_final: _prev: {
        picom = optimizePkg inputs.picom.packages."${system}".picom;
      })
    ];
  };
}
