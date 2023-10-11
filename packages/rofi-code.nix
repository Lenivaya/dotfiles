{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule rec {
  pname = "rofi-code";
  version = "master";

  src = fetchFromGitHub {
    owner = "Coffelius";
    repo = "rofi-code";
    rev = version;
    hash = "sha256-mfMiraadJRqtMB+4yzeDozDApPAUHnLiTgEgRjPXJqs=";
  };

  vendorHash = "sha256-Yp2tBZWHc/xjp1rpJEAfz8nNo4Wtu66rdwCEkfeLq3I=";

  ldflags = ["-s" "-w"];

  meta = with lib; {
    description = "Use rofi to quickly open VSCode or Codium workspaces";
    homepage = "https://github.com/Coffelius/rofi-code";
    license = licenses.mit;
    maintainers = with maintainers; [];
    mainProgram = "rofi-code";
  };
}
