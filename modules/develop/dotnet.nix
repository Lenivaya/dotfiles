{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.dotnet;
in {
  options.modules.dev.dotnet = with types; {
    enable = mkBoolOpt false;
    dotnetPkg = mkOpt package pkgs.dotnet-sdk;
    dotnetAspNetPkg = mkOpt package pkgs.dotnet-aspnetcore;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      cfg.dotnetPkg
      cfg.dotnetAspNetPkg
      msbuild
      omnisharp-roslyn
    ];

    env = {
      DOTNET_ROOT = "${cfg.dotnetPkg}";
      DOTNET_CLI_HOME = "$XDG_DATA_HOME"; # dotnet cli appends the path with .dotnet :(
      NUGET_PACKAGES = "$XDG_DATA_HOME/NuGet/packages";
      NUGET_HTTP_CACHE_PATH = "$XDG_DATA_HOME/NuGet/v3-cache";
      NUGET_PLUGINS_CACHE_PATH = "$XDG_DATA_HOME/NuGet/plugins-cache";

      PATH = ["$DOTNET_CLI_HOME/.dotnet/tools"];
    };
  };
}
