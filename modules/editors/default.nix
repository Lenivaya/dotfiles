{ config, options, lib, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.editors;
in {
  options.modules.editors = { default = mkOpt types.str "vim"; };

  config =
    mkIf (cfg.default != null && config.services.emacs.defaultEditor == false) {
      env.EDITOR = cfg.default;
    };
}
