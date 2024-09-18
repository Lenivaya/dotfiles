# "saying java is good because it runs on all systems is like saying
# anal sex is good because it works on all species"
# - sun tzu

{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.java;
in
{
  options.modules.dev.java.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.java = {
      # adds JAVA_HOME to the global environment
      # by sourcing the jdk’s setup-hook on shell init
      # slightly slows down the shell since the java path needs
      # to be realised
      enable = true;

      # jdk package to use
      package = pkgs.jre;

      # whether to enable binfmt for executing
      # java jar’s and classes. This can be a security
      # exploit.
      # binfmt = lib.mkForce false;
    };
  };
}
