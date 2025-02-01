{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.fish;
  inherit (config.dotfiles) outOfStoreConfigDir;
in
{
  options.modules.shell.fish = with types; {
    enable = mkBoolOpt false;
    default = mkBoolOpt false;
    aliases = mkOpt (attrsOf (either str path)) { };
    package = mkOpt package pkgs.fish;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # users.defaultUserShell = pkgs.fish;
      # programs.zsh = {
      #   initExtra = ''
      #     if [[ $(ps -o command= -p "$PPID" | awk '{print $1}') != 'fish' ]]
      #     then
      #         exec fish -l
      #     fi
      #   '';
      # };
      # programs.bash = {
      #   interactiveShellInit = ''
      #     if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
      #     then
      #       shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
      #       exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
      #     fi
      #   '';
      # };
      #

      modules = {
        shell = {
          fzf = enabled;
        };
      };

      programs.fish = enabled // {
        interactiveShellInit = mkForce "";
        useBabelfish = true;
      };
      environment.systemPackages = with pkgs; [
        cfg.package
        lsd
      ];

      # Embrace impurity and imperfection.
      system.userActivationScripts.linkFishConfig = linkIfNotExist "~/.config/fish" "${outOfStoreConfigDir}/fish";
    }

    (mkIf cfg.default {
      users.defaultUserShell = cfg.package;
    })
  ]);
}
