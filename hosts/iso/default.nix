{ pkgs, lib, ... }:
with lib;
with lib.my;
{
  imports = [
    ../common.nix
    ../iso.nix
  ];

  modules = {
    shell = {
      zsh = enabled;
      tmux = enabled;
      gnupg = enabled;
      direnv = enabled;
      git = enabled;
    };

    editors = { };

    dev = {
      nix = enabled;
    };

    services = {
      ssh = enabled;
      keyd = enabled;
    };

    programs = {
      nix-helper = enabled;
    };

    hardware = {
      audio = disabled;
    };

    zram = enabled;
    fast-networking = enabled;
  };

  nix.gc.automatic = mkForce false;

  boot.kernelPackages =
    let
      kernel' = pkgs.linuxPackages; # lts
    in
    mkForce kernel';

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
    "nohibernate"
    # https://wiki.archlinux.org/title/improving_performance#Watchdogs
    "nowatchdog"
    "kernel.nmi_watchdog=0"
    "msr.allow_writes=on"
  ];

  environment.shellAliases = {
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
  };

  # Dirty hack to have hosts file modifiable
  # (will be discarded on config change or reboot) [1]
  #
  # [1]: https://discourse.nixos.org/t/a-fast-way-for-modifying-etc-hosts-using-networking-extrahosts/4190/3
  environment.etc.hosts.mode = "0644";

  services.journald.extraConfig = ''
    SystemMaxUse=50M
    RuntimeMaxUse=10M
    SystemMaxFileSize=50M
  '';

  systemd.coredump.extraConfig = ''
    Storage=none
    ProcessSizeMax=0
  '';

  nix.settings = {
    system-features = [
      "gccarch-x86-64-v3"
      "gccarch-x86-64-v4"
      "big-parallel"
    ];
  };
}
