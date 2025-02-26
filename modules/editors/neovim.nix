{
  config,
  lib,
  pkgs,
  # inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.editors.neovim;
  inherit (config.dotfiles) outOfStoreConfigDir;
in
{
  options.modules.editors.neovim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      neovim
      lua
      ripgrep
    ];

    system.userActivationScripts.linkNvimConfig = linkIfNotExist "~/.config/nvim" "${outOfStoreConfigDir}/nvim";
  };
}
