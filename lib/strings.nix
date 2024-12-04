_: rec {
  comcat = builtins.concatStringsSep ",";
  spaceConcat = list: builtins.concatStringsSep " " list;

  isEmptyString = str: str == "";
  notEmptyString = str: !(isEmptyString str);

  # Helper for shell environment variables that need proper expansion
  # Example: envVar "XDG_CONFIG_HOME" -> "${XDG_CONFIG_HOME}"
  envVar = name: "$\{${name}}";
}
