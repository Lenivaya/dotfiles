{config, ...}: {
  programs.ccache.enable = true;
  nix.settings.extra-sandbox-paths = [config.programs.ccache.cacheDir];
}
