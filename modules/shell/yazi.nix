{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.yazi;
  inherit (config.dotfiles) outOfStoreConfigDir;
in
{
  options.modules.shell.yazi.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      yazi
      exiftool # general file info
      glow
      miller
      ouch
    ];

    # Embrace impurity and imperfection.
    system.userActivationScripts.linkYaziConfig = linkIfNotExist "~/.config/yazi" "${outOfStoreConfigDir}/yazi";
  };
}
