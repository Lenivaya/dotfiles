_: rec {
  comcat = builtins.concatStringsSep ",";

  isEmptyString = str: str == "";
  notEmptyString = str: !(isEmptyString str);
}
