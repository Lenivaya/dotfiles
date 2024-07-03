{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.warp;
  warpScript = pkgs.writeScriptBin "warp" ''
    if systemctl is-active --quiet warp-svc; then
        warp-cli disconnect
        systemctl stop warp-svc.service
    else
        systemctl start warp-svc.service
        sleep 1
        warp-cli connect
    fi
  '';
in {
  options.modules.services.warp.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    systemd.services.warp-svc = mkGraphicalService {
      description = "Cloudfare warp daemon";

      path = with pkgs; [cloudflare-warp];
      script = "warp-svc";
    };

    user.packages = with pkgs; [cloudflare-warp warpScript];
  };
}
