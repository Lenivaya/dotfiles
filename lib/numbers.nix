_: {
  pow = base: exponent:
    assert (builtins.isInt base) && (builtins.isInt exponent);
    assert exponent > 0;
      builtins.foldl' (x: _: x * base) 1 (builtins.genList _ exponent);

  MHz = x: x * 1000;
}
