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
    user.packages =
      # let
      # in
      # neovim' = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
      with pkgs; [
        neovim
      ];

    system.userActivationScripts.linkNvimConfig = linkIfNotExist "~/.config/nvim" "${outOfStoreConfigDir}/nvim";
  };
}
