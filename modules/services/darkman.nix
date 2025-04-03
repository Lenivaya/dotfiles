{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.darkman;
in
{
  options.modules.services.darkman.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.services.darkman = enabled // {
      darkModeScripts = {
        kitty = ''
          ${pkgs.kitty}/bin/kitten themes dark-theme.auto
          ${pkgs.procps}/bin/kill -SIGUSR1 $(${pkgs.procps}/bin/pidof kitty)
        '';
        vscode = ''
          VSCODE_SETTINGS="$HOME/.config/dotfiles/config/vscode/settings.json"
          if [ -f "$VSCODE_SETTINGS" ]; then
            DARK_THEME=$(${pkgs.jq}/bin/jq -r '.["workbench.preferredDarkColorTheme"] // "Min Tomorrow Dark"' "$VSCODE_SETTINGS")
            ${pkgs.jq}/bin/jq ".\"workbench.colorTheme\" = \"$DARK_THEME\"" "$VSCODE_SETTINGS" | ${pkgs.moreutils}/bin/sponge "$VSCODE_SETTINGS"
          fi
        '';
      };
      lightModeScripts = {
        kitty = ''
          ${pkgs.kitty}/bin/kitten themes 'Solarized Light'
          ${pkgs.procps}/bin/kill -SIGUSR1 $(${pkgs.procps}/bin/pidof kitty)
        '';
        vscode = ''
          VSCODE_SETTINGS="$HOME/.config/dotfiles/config/vscode/settings.json"
          if [ -f "$VSCODE_SETTINGS" ]; then
            LIGHT_THEME=$(${pkgs.jq}/bin/jq -r '.["workbench.preferredLightColorTheme"] // "Solarized Light"' "$VSCODE_SETTINGS")
            ${pkgs.jq}/bin/jq ".\"workbench.colorTheme\" = \"$LIGHT_THEME\"" "$VSCODE_SETTINGS" | ${pkgs.moreutils}/bin/sponge "$VSCODE_SETTINGS"
          fi
        '';
      };
    };
  };
}
