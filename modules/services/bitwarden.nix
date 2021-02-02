{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.bitwarden;
in {
  options.modules.services.bitwarden = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.bitwarden_rs.enable = true;
    user.extraGroups = [ "bitwarden_rs" ];
  };
}
