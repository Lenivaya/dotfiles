{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.kuber;
in
{
  options.modules.dev.kuber.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      kubectl
      kubernetes-helm
      k9s
      minikube
      kustomize
      kubectx # Includes kubens
      # Add other useful tools here, e.g.:
      kind
      stern
    ];
  };
}
