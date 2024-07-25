{ lib, ... }:
let
  inherit (lib)
    mkOption
    types
    foldr
    unique
    mkIf
    ;
in
rec {
  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' =
    type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt =
    default:
    mkOption {
      inherit default;
      type = types.bool;
      example = true;
    };

  disabled = {
    enable = false;
  };
  enabled' = otherThings: { enable = true; } // otherThings;
  enabled = enabled' { };

  ifEnabled = cfg: mkIf cfg.enable;

  valueForEach = names: value: foldr (name: acc: acc // { "${name}" = value; }) { } (unique names);
}
