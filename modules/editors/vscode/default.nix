{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  inherit (config.dotfiles) configDir;
  inherit (config) modules;
in
{
  options.modules.editors.vscode = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.nix-vscode-extensions.overlays.default ];

    # embrace the impurnes, make it more usable
    system.userActivationScripts.linkVscodeConfig = ''
      ${linkIfNotExist "~/.config/Code/User/settings.json" "${configDir}/vscode/settings.json"}
      ${linkIfNotExist "~/.config/Code/User/keybindings.json" "${configDir}/vscode/keybindings.json"}

      ${linkIfNotExist "~/.config/Cursor/User/settings.json" "${configDir}/vscode/settings.json"}
      ${linkIfNotExist "~/.config/Cursor/User/keybindings.json" "${configDir}/vscode/keybindings.json"}
    '';

    home.programs.vscode = enabled // {
      package =
        let
          vscode' =
            (pkgs.vscode.override {
              # inherit (chromeCfg) commandLineArgs;
            }).fhs;
        in
        vscode';

      mutableExtensionsDir = true;
      extensions =
        with pkgs.vscode-marketplace;
        [
          bodil.file-browser

          editorconfig.editorconfig
          mikestead.dotenv

          dbaeumer.vscode-eslint
          esbenp.prettier-vscode
          davidanson.vscode-markdownlint

          timonwong.shellcheck

          wakatime.vscode-wakatime
          alefragnani.project-manager
          # tomoki1207.pdf

          vincaslt.highlight-matching-tag
          spywhere.guides

          vscodevim.vim
          vspacecode.whichkey
          vspacecode.vspacecode
          jacobdufault.fuzzy-search
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

          # :git
          codezombiech.gitignore
          donjayamanne.githistory
          eamodio.gitlens
          github.vscode-pull-request-github
          mhutchie.git-graph
          kahole.magit

          # :lang
          # shd101wyy.markdown-preview-enhanced
          yzhang.markdown-all-in-one
          castwide.solargraph
          cschlosser.doxdocgen
          ms-vscode.hexeditor
          reditorsupport.r
          tamasfe.even-better-toml
          vscode-org-mode.org-mode
          skellock.just

          mustafamohamad.min-tomorrow-theme

          ryuta46.multi-command
        ]
        ++ optionals modules.desktop.media.documents.latex.enable [
          james-yu.latex-workshop
          efoerster.texlab
        ]
        ++ optionals modules.dev.nix.enable [
          jnoortheen.nix-ide
          arrterian.nix-env-selector
        ]
        ++ optionals modules.dev.go.enable [ rust-lang.rust-analyzer ]
        ++ optionals modules.dev.rust.enable [ golang.go ]
        ++ optionals modules.dev.node.enable [ mgmcdermott.vscode-language-babel ]
        ++ optionals modules.dev.haskell.enable [
          haskell.haskell
          justusadam.language-haskell
        ]
        ++ optionals modules.dev.cc.enable [
          bazelbuild.vscode-bazel
          ms-vscode.cpptools
          jeff-hykin.better-cpp-syntax
          mitaki28.vscode-clang
          ms-vscode.cmake-tools
          twxs.cmake
        ]
        ++ optionals modules.dev.python.enable [
          ms-python.python
          ms-toolsai.jupyter
        ]
        ++ optionals modules.dev.dotnet.enable [
          # (ms-dotnettools.csdevkit.overrideAttrs (_super: _a: {sourceRoot = ".";}))
          ms-dotnettools.csdevkit
        ];
    };
  };
}
