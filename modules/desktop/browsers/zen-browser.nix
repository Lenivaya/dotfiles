# https://docs.zen-browser.app/guides/manage-profiles
# https://github.com/zen-browser/desktop/discussions/881#discussioncomment-11217246
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.zen-browser;
in
{
  options.modules.desktop.browsers.zen-browser = with types; {
    enable = mkBoolOpt false;
    package = mkOpt package inputs.zen-browser.packages."${pkgs.system}".default;
    executable = mkOpt str "zen";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.nur.overlays.default ];

    env.XDG_DESKTOP_DIR = "$HOME"; # prevent zen-browser creating ~/Desktop

    user.packages = with pkgs; [
      cfg.package

      profile-cleaner

      (makeDesktopItem {
        name = "zenbrowser-private";
        desktopName = "Zenbrwoser (Private)";
        genericName = "Open a private Zenbrwoser window";
        icon = "ZenBrowser";
        exec = "${cfg.executable} --private-window";
        categories = [ "Network" ];
      })
    ];

    environment.sessionVariables = {
      MOZ_WEBRENDER = 1;
      MOZ_USE_XINPUT2 = "1";
    };
  };
}
