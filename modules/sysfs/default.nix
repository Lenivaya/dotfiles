{ config, lib, ... }:

with lib;

# Adapted off sysctl.nix

{
  options.boot.kernel.sysfs = mkOption {
    default = { };
    type = types.attrs;
  };

  config = {
    systemd.tmpfiles.rules = let
      formatString = x:
        if isBool x then
          if x == false then "0" else "1"
        else
          builtins.toString x;

      toRule = path: value: "w ${path} - - - - ${formatString value}";

      isLeaf = x: !(isAttrs x);
      cascadeTree = root: prefix:
        if isLeaf root then
          toRule prefix root
        else
          mapAttrsToList (n: v: cascadeTree v "${prefix}/${n}") root;
    in flatten (cascadeTree config.boot.kernel.sysfs "");
  };
}
