{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.dotnet;
  sdk' = pkgs.dotnetCorePackages.combinePackages cfg.dotnetPkgsSdks;
in {
  options.modules.dev.dotnet = with types; {
    enable = mkBoolOpt false;
    dotnetPkgsSdks = mkOpt (listOf package) [
      pkgs.dotnet-sdk
      # dotnetCorePackages.sdk_8_0 this is to be setted
    ];
    otherPkgs = mkOpt (listOf package) [];
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        sdk'
        # msbuild
        omnisharp-roslyn
        csharpier
      ]
      ++ cfg.dotnetPkgsSdks
      ++ cfg.otherPkgs;

    env = {
      DOTNET_ROOT = "${sdk'}";
      DOTNET_CLI_HOME = "$XDG_DATA_HOME"; # dotnet cli appends the path with .dotnet :(
      NUGET_PACKAGES = "$XDG_DATA_HOME/NuGet/packages";
      NUGET_HTTP_CACHE_PATH = "$XDG_DATA_HOME/NuGet/v3-cache";
      NUGET_PLUGINS_CACHE_PATH = "$XDG_DATA_HOME/NuGet/plugins-cache";

      PATH = ["$DOTNET_CLI_HOME/.dotnet/tools"];
    };
  };
}
