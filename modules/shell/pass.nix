{
  config,
  options,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.pass;
in {
  options.modules.shell.pass.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (pass.withExtensions (exts:
        with exts;
          [
            pass-otp
            pass-genphrase
            pass-audit
            pass-import
            pass-update
          ]
          ++ (
            if config.modules.shell.gnupg.enable
            then [exts.pass-tomb]
            else []
          )))
    ];
    env.PASSWORD_STORE_DIR = "$HOME/.secrets/password-store";
  };
}
