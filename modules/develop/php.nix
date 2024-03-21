# https://shyim.me/blog/devenv-compose-developer-environment-for-php-with-nix/
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.php;
in {
  options.modules.dev.php = with types; {
    enable = mkBoolOpt false;
    package = mkOpt package pkgs.php;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; let
      php' = cfg.package;
    in
      [
        php'
        phpactor
        nodePackages.intelephense
      ]
      ++ (with php'.packages; [
        composer
        php-cs-fixer
        phpcbf
        psalm
        phpstan
      ]);
  };
}
