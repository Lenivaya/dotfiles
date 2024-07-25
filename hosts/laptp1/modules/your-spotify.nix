{ lib, ... }:
with lib;
with my;
{
  # services.your_spotify =
  #   enabled
  #   // {
  #     spotifySecretFile = config.age.secrets.your_spotify_secret.path;
  #     # nginxVirtualHost = "local.self-hosted.com";
  #     settings = {
  #       PORT = 4500;
  #       SPOTIFY_PUBLIC = "a25a6c24950d4dc6b5170315e79331ac";
  #       API_ENDPOINT = "";
  #     };
  #   };
}
