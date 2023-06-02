{
  config,
  options,
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.vscode;
  inherit (config.dotfiles) configDir;
in {
  options.modules.editors.vscode = with types; {
    enable = mkBoolOpt false;
  };

  # Min tomorrow night theme
  config = mkIf cfg.enable {
    # HACK to make config writable
    home.activation.boforeCheckLinkTargets = {
      after = [];
      before = ["checkLinkTargets"];
      data = ''
        userDir=~/.config/Code/User
        rm -rf $userDir/settings.json
        rm -rf $userDir/keybindings.json
      '';
    };

    home.activation.afterWriteBoundary = {
      after = ["writeBoundary"];
      before = [];
      data = ''
        userDir=~/.config/Code/User
        rm -rf $userDir/settings.json
        rm -rf $userDir/keybindings.json
        cat \
          ${(pkgs.formats.json {}).generate "blabla"
          config.home.programs.vscode.userSettings} \
          > $userDir/settings.json
        cat \
          ${(pkgs.formats.json {}).generate "blabla"
          config.home.programs.vscode.keybindings} \
          > $userDir/keybindings.json
      '';
    };

    nixpkgs.overlays = [inputs.nix-vscode-extensions.overlays.default];

    home.programs.vscode = {
      enable = true;
      mutableExtensionsDir = true;
      userSettings = import "${configDir}/vscode/settings.nix" {
        inherit pkgs;
        inherit config;
      };
      keybindings = import "${configDir}/vscode/keybindings.nix";
      extensions = with pkgs.vscode-extensions;
      with inputs.nix-vscode-extensions.extensions.${pkgs.system}.vscode-marketplace; [
        bodil.file-browser

        editorconfig.editorconfig
        codezombiech.gitignore
        mikestead.dotenv
        shd101wyy.markdown-preview-enhanced
        yzhang.markdown-all-in-one

        dbaeumer.vscode-eslint
        esbenp.prettier-vscode
        davidanson.vscode-markdownlint

        jnoortheen.nix-ide
        kamadorueda.alejandra

        timonwong.shellcheck

        WakaTime.vscode-wakatime
        alefragnani.project-manager
        # tomoki1207.pdf

        vincaslt.highlight-matching-tag
        spywhere.guides

        vscodevim.vim
        vspacecode.whichkey
        vspacecode.vspacecode
        ms-vscode.vs-keybindings

        aaron-bond.better-comments
        alefragnani.bookmarks
        christian-kohler.path-intellisense
        formulahendry.auto-rename-tag
        gruntfuggly.todo-tree
        mkxml.vscode-filesize
        ms-azuretools.vscode-docker
        ms-vscode-remote.remote-containers
        ms-vscode-remote.remote-ssh
        ms-vsliveshare.vsliveshare
        nhoizey.gremlins
        peterschmalfeldt.explorer-exclude
        quicktype.quicktype
        stylelint.vscode-stylelint
        usernamehw.errorlens

        donjayamanne.githistory
        eamodio.gitlens
        github.vscode-pull-request-github
        mhutchie.git-graph
        kahole.magit

        alexcvzz.vscode-sqlite
        bazelbuild.vscode-bazel
        castwide.solargraph
        cschlosser.doxdocgen
        golang.go
        haskell.haskell
        justusadam.language-haskell
        james-yu.latex-workshop
        ms-vscode.cpptools
        jeff-hykin.better-cpp-syntax
        mitaki28.vscode-clang
        ms-vscode.cmake-tools
        twxs.cmake
        ms-vscode.hexeditor
        kdarkhan.mips
        mgmcdermott.vscode-language-babel
        ms-python.python
        ms-python.vscode-pylance
        ms-toolsai.jupyter
        reditorsupport.r
        rust-lang.rust-analyzer
        samuelcolvin.jinjahtml
        scala-lang.scala
        slevesque.shader
        tamasfe.even-better-toml

        mustafamohamad.min-tomorrow-theme

        ryuta46.multi-command
      ];
    };
  };
}
