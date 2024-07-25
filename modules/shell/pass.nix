{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.pass;
in
{
  options.modules.shell.pass.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages =
      with pkgs;
      let
        pass' = pass.withExtensions (
          exts:
          with exts;
          [
            pass-otp
            pass-genphrase
            # pass-audit
            pass-import
            pass-update
            pass-file
            pass-checkup
          ]
          ++ optional config.modules.shell.gnupg.enable exts.pass-tomb
        );
      in
      [ pass' ];

    env.PASSWORD_STORE_DIR = "$HOME/.secrets/password-store";
  };
}
