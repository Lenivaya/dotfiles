_: rec {
  comcat = builtins.concatStringsSep ",";
  spaceConcat = list: builtins.concatStringsSep " " list;

  isEmptyString = str: str == "";
  notEmptyString = str: !(isEmptyString str);
}
