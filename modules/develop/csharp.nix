{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.csharp;
  dotnet-package = pkgs.dotnetCorePackages.sdk_6_0;
in {
  options.modules.dev.csharp.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      mono
      msbuild
      dotnet-package
      dotnet-aspnetcore

      omnisharp-roslyn
    ];

    env = {
      DOTNET_ROOT = "${dotnet-package.out}";
      DOTNET_CLI_HOME = "$XDG_DATA_HOME"; # dotnet cli appends the path with .dotnet :(
      NUGET_PACKAGES = "$XDG_DATA_HOME/NuGet/packages";
      NUGET_HTTP_CACHE_PATH = "$XDG_DATA_HOME/NuGet/v3-cache";
      NUGET_PLUGINS_CACHE_PATH = "$XDG_DATA_HOME/NuGet/plugins-cache";

      PATH = ["$DOTNET_CLI_HOME/.dotnet/tools"];
    };
  };
}
