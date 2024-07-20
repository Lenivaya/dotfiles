{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.espanso;
  inherit (config.dotfiles) configDir;
in {
  options.modules.services.espanso.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [espanso];

    services.espanso = enabled;
    systemd.user.services.espanso.path = with pkgs; [curl coreutils]; # FIXME still doesn't work
    systemd.user.services.espanso.serviceConfig.PassEnvironment = "PATH";

    # I want it to be editable
    system.userActivationScripts.linkEspansoConfig =
      linkIfNotExist "~/.config/espanso" "${configDir}/espanso";

    # May be useful in future [1],
    # but for now it's pretty shitty solution [2]
    # [1]: https://github.com/nix-community/home-manager/issues/3514
    # [2]: https://github.com/nix-community/home-manager/issues/2980
    # home.configFile."espanso".source =
    #   config.lib.file.mkOutOfStoreSymlink "${configDir}/espanso";
  };
}
