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

    aliases = mkOpt (attrsOf (either str path)) { };
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.fish;
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

    environment.systemPackages = with pkgs; [
      # fishPlugins.done
      # fishPlugins.fzf-fish
      # fishPlugins.forgit
      fzf
      # fishPlugins.grc
      # grc
    ];

    programs.fish = enabled // {
      vendor = {
        config.enable = true;
        completions.enable = true;
        functions.enable = true;
      };
    };

    # Embrace impurity and imperfection.
    system.userActivationScripts.linkFishConfig = linkIfNotExist "~/.config/fish" "${outOfStoreConfigDir}/fish";
  };
}
