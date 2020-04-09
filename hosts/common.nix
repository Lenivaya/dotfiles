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
    sshfs
    xorg.xkill # Xorg
  ];

  my.username = "leniviy";
  my.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "adbusers" "docker" ];
    shell = pkgs.zsh;
  };

  # Auto-mount
  programs = {
    gnome-disks.enable = true;
    udevil.enable = true;
  };

}
