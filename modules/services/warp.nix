{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.warp;
  warpScript = pkgs.writeScriptBin "warp" ''
    enabled=$(
      warp-cli status | grep -qw "Connected"
      echo $?
    )

    if [ $enabled -eq 0 ]; then
      warp-cli disconnect
      systemctl stop warp-svc.service
    else
      systemctl start warp-svc.service
      sleep 1
      warp-cli connect
    fi
  '';
in {
  options.modules.services.warp = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    systemd.services.warp-svc = {
      description = "Cloudfare warp daemon";
      after = ["graphical-session-pre.target"];
      partOf = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];

      path = with pkgs; [cloudflare-warp];
      script = "warp-svc";
    };

    user.packages = with pkgs; [
      cloudflare-warp
      warpScript
    ];
  };
}
