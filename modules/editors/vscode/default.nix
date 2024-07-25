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
  chromeCfg = config.modules.desktop.browsers.chromium;
  cfg = config.modules.editors.vscode;
  inherit (config.dotfiles) configDir;
  inherit (config) modules;
in
{
  options.modules.editors.vscode = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # imports = [
    #   # Source: https://gist.github.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa
    #   # Make vscode settings writable

    #   (import (builtins.fetchurl {
    #     url = "https://gist.githubusercontent.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa/raw/41e569ba110eb6ebbb463a6b1f5d9fe4f9e82375/mutability.nix";
    #     sha256 = "4b5ca670c1ac865927e98ac5bf5c131eca46cc20abf0bd0612db955bfc979de8";
    #   }) {inherit config lib;})

    #   (import (builtins.fetchurl {
    #     url = "https://gist.githubusercontent.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa/raw/41e569ba110eb6ebbb463a6b1f5d9fe4f9e82375/vscode.nix";
    #     sha256 = "fed877fa1eefd94bc4806641cea87138df78a47af89c7818ac5e76ebacbd025f";
    #   }) {inherit config lib pkgs;})
    # ];
    home.activation =
      let
        sysDir =
          if pkgs.stdenv.hostPlatform.isDarwin then
            "${config.home.homeDirectory}/Library/Application Support"
          else
            "${config.user.home}/.config";
        userConfigPath = "${sysDir}/Code/User";

        mkTmp = file: "${file}.tmp";

        userSettings = "${userConfigPath}/settings.json";
        userKeybindings = "${userConfigPath}/keybindings.json";

        userSettings' = mkTmp userSettings;
        userKeybindings' = mkTmp userKeybindings;
      in
      {
        removeExistingVSCodeSettings = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
          rm -rf "${userSettings}"
          rm -rf "${userKeybindings}"
        '';

        overwriteVSCodeSymlink = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
          cat "${userSettings}" > ${userSettings'}
          cat "${userKeybindings}" > ${userKeybindings'}

          rm -rf "${userSettings}"
          rm -rf "${userKeybindings}"

          cat "${userSettings'}" > ${userSettings}
          cat "${userKeybindings'}" > ${userKeybindings}
        '';
      };

    nixpkgs.overlays = [ inputs.nix-vscode-extensions.overlays.default ];

    home.programs.vscode = enabled // {
      package =
        let
          vscode' = pkgs.vscode.override { inherit (chromeCfg) commandLineArgs; };
        in
        vscode';

      keybindings = import "${configDir}/vscode/keybindings.nix";
      userSettings = import "${configDir}/vscode/settings.nix" { inherit lib pkgs config; };

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
