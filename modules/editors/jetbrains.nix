{
  config,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.jetbrains;
in {
  options.modules.editors.jetbrains = with types; {
    enable = mkBoolOpt false;
    packages = mkOpt (listOf package) [];
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [inputs.jetbrains.overlays.default];
    user.packages = cfg.packages;
  };
}
