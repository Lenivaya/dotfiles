{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.psd;
in
{
  options.modules.services.psd.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.psd = enabled // {
      resyncTimer = "30min";
    };

    environment.systemPackages = with pkgs; [
      profile-sync-daemon
    ];

    home.configFile."psd/psd.conf".text = ''
      USE_OVERLAYFS="yes"
      USE_BACKUPS="no"
      BROWSERS="firefox"
    '';

    security.sudo = enabled // {
      extraRules =
        let
          mkRule = pkg: cmd: rules: [
            {
              command = "${pkg}/bin/${cmd}";
              options = rules;
            }
            {
              command = "/run/current-system/sw/bin/${cmd}";
              options = rules;
            }
          ];
          mkNoPwd = pkg: cmd: mkRule pkg cmd [ "NOPASSWD" ];
        in
        [
          {
            commands = mkMerge [
              (mkNoPwd pkgs.profile-sync-daemon "psd-overlay-helper")
            ];
            groups = [ "wheel" ];
          }
        ];
    };
  };
}
