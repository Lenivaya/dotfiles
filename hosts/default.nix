{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vim
    binutils
    coreutils
    killall
    wget
    curl
    git
    xclip
    xorg.xkill # Xorg
  ];

  my.username = "leniviy";
  my.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "adbusers" "docker" ];
    shell = pkgs.zsh;
  };

}
