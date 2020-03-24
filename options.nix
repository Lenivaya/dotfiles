{ config, options, lib, ... }:

with lib;
let
  mkOptionStr = value:
    mkOption {
      type = types.str;
      default = value;
    };
in {
  imports = [ <home-manager/nixos> ];

  ## Personal settings
  options.my.username = mkOptionStr "leniviy";
  options.my.email = mkOptionStr "xocada@gmail.com";

  ## Convenience aliases
  options.my.home =
    mkOption { type = options.home-manager.users.type.functor.wrapped; };
  config.home-manager.users.${config.my.username} =
    mkAliasDefinitions options.my.home;

  options.my.user = mkOption { type = types.submodule; };
  config.users.users.${config.my.username} = mkAliasDefinitions options.my.user;

  options.my.packages = mkOption {
    type = types.listOf types.package;
    description = "The set of packages to appear in the user environment.";
  };
  config.my.user.packages = config.my.packages;

  ## Shell/Environment
  options.my.env = mkOption {
    type = with types;
      attrsOf (either (either str path) (listOf (either str path)));
    apply = mapAttrs (n: v:
      if isList v then
        concatMapStringsSep ":" (x: toString x) v
      else
        (toString v));
  };
  options.my.init = mkOption {
    type = types.lines;
    description = ''
      An init script that runs after the environment has been rebuilt or
      booted. Anything done here should be idempotent and inexpensive.
    '';
  };
  ## PATH should always start with its old value
  config.my.env.PATH = [ ./bin "$PATH" ];
  config.environment.extraInit = let
    exportLines = mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env;
  in ''
    ${concatStringsSep "\n" exportLines}
    ${config.my.init}
  '';

}
