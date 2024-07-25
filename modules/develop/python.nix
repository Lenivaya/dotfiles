{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.python;
in
{
  options.modules.dev.python.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      poetry
      ruff # lint

      pyright

      (python312.withPackages (
        ps: with ps; [
          # python-lsp-server

          pip
          ipython
          black
          isort
          setuptools

          pylint
          pyflakes

          # python-lsp-ruff
        ]
      ))
    ];

    env = {
      IPYTHONDIR = "$XDG_CONFIG_HOME/ipython";
      PIP_CONFIG_FILE = "$XDG_CONFIG_HOME/pip/pip.conf";
      PIP_LOG_FILE = "$XDG_DATA_HOME/pip/log";
      PYLINTHOME = "$XDG_DATA_HOME/pylint";
      PYLINTRC = "$XDG_CONFIG_HOME/pylint/pylintrc";
      PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
      PYTHON_EGG_CACHE = "$XDG_CACHE_HOME/python-eggs";
      JUPYTER_CONFIG_DIR = "$XDG_CONFIG_HOME/jupyter";
    };

    environment.shellAliases = {
      py = "python";
      py2 = "python2";
      py3 = "python3";
      po = "poetry";
      ipy = "ipython --no-banner";
      ipylab = "ipython --pylab=qt5 --no-banner";
    };
  };
}
