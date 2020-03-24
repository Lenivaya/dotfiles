{ pkgs, ... }:

{
  home-manager.users.leniviy = { pkgs, ... }: {
    home.packages = with pkgs; [
      (python37.withPackages(ps: with ps;
        [ pip virtualenvwrapper conda # Env
          pytest nose                # Tests
          black                      # Formatter
          pylint
          python-language-server
          pyls-black pyls-isort pyls-mypy
          grip                       # Grip -- GitHub Readme Instant Preview
        ]
      ))
    ];
  };
}
