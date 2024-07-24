{lib, ...}:
with lib; {
  config.boot = {
    # whether to enable support for Linux MD RAID arrays
    # I don't know why this defaults to true, how many people use RAID anyway?
    # also on > 23.11, this will throw a warning if neither MAILADDR nor PROGRAM are set
    swraid.enable = mkDefault false;
  };
}
