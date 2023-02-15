{
  pkgs,
  config,
  ...
}:
with pkgs;
# let
#   # JAVA_HOME = "${openjdk}/lib/openjdk";
# in
  {
    # Vim
    "vim.easymotion" = true;
    "vim.incsearch" = true;
    "vim.useSystemClipboard" = true;
    "vim.useCtrlKeys" = true;
    "vim.hlsearch" = true;
    # "vim.leader" = "<space>";
    "vim.sneak" = true;
    "vim.camelCaseMotion.enable" = true;
    "vim.insertModeKeyBindings" = [
      {
        "before" = ["j" "j"];
        "after" = ["<Esc>"];
      }
    ];
    "vim.normalModeKeyBindingsNonRecursive" = [
      {
        "before" = ["<Esc>"];
        "commands" = [":nohl"];
      }
      {
        "before" = ["<space>"];
        "commands" = ["vspacecode.space"];
      }
      {
        "before" = [
          ","
        ];
        "commands" = [
          "vspacecode.space"
          {
            "command" = "whichkey.triggerKey";
            "args" = "m";
          }
        ];
      }
    ];
    "vim.visualModeKeyBindingsNonRecursive" = [
      {
        "before" = ["<space>"];
        "commands" = ["vspacecode.show"];
      }
      {
        "before" = [
          ","
        ];
        "commands" = [
          "vspacecode.space"
          {
            "command" = "whichkey.triggerKey";
            "args" = "m";
          }
        ];
      }
    ];

    # Editor
    "editor.mouseWheelZoom" = true;
    "diffEditor.ignoreTrimWhitespace" = false;
    "editor.cursorBlinking" = "smooth";
    "editor.cursorSmoothCaretAnimation" = true;
    # "editor.fontFamily" = "'Anonymous Pro', 'Recursive Mono Linear Static', 'Rec Mono Linear', 'IBM Plex Mono'";
    # "editor.fontLigatures" = "'ss01','ss05','ss08','ss09','ss20'";
    # "editor.fontSize" = 13;
    # "editor.fontWeight" = "normal";
    "editor.fontLigatures" = true;
    "editor.lineHeight" = 20;
    "editor.lineNumbers" = "relative";
    "editor.multiCursorModifier" = "ctrlCmd";
    "editor.quickSuggestions" = {
      "other" = true;
      "comments" = true;
      "strings" = true;
    };
    "editor.renderIndentGuides" = false;
    "editor.renderWhitespace" = "none";
    "editor.rulers" = [
      80
      100
    ];
    "editor.smoothScrolling" = true;
    "editor.snippetSuggestions" = "top";
    "editor.suggest.localityBonus" = true;
    "editor.tabSize" = 2;
    "editor.wordWrap" = "off";
    "editor.minimap.showSlider" = "always";
    "workbench.colorCustomizations" = {
      "activityBarBadge.background" = "#616161";
      "list.activeSelectionForeground" = "#616161";
      "list.inactiveSelectionForeground" = "#616161";
      "list.highlightForeground" = "#616161";
      "scrollbarSlider.activeBackground" = "#61616150";
      "editorSuggestWidget.highlightForeground" = "#616161";
      "textLink.foreground" = "#616161";
      "progressBar.background" = "#616161";
      "pickerGroup.foreground" = "#616161";
      "tab.activeBorder" = "#616161";
      "notificationLink.foreground" = "#616161";
      "editorWidget.border" = "#616161";
    };
    "workbench.editorAssociations" = [
      {
        "viewType" = "jupyter-notebook";
        "filenamePattern" = "*.ipynb";
      }
    ];
    "workbench.list.smoothScrolling" = true;
    "workbench.iconTheme" = "vs-seti";
    "workbench.sideBar.location" = "right";
    "workbench.tree.expandMode" = "singleClick";
    "breadcrumbs.enabled" = true;
    "window.menuBarVisibility" = "toggle";
    "window.titleBarStyle" = "native";
    "files.associations" = {
      "bspwmrc" = "shellscript";
      "sxhkdrc" = "shellscript";
      "*.pug" = "jade";
      "*.toml" = "ini";
      "*.conkyrc" = "lua";
      "*.xmobarrc" = "haskell";
      ".Xresources" = "shellscript";
    };
    "files.enableTrash" = false;
    "files.exclude" = {
      "**/.git" = false;
      "**/.svn" = true;
      "**/.hg" = true;
      "**/CVS" = true;
      "**/.DS_Store" = false;
      "**/*.olean" = true;
    };
    "files.insertFinalNewline" = true;
    "files.trimTrailingWhitespace" = true;
    "files.watcherExclude" = {
      "**/.git/objects/**" = true;
      "**/.git/subtree-cache/**" = true;
      "**/node_modules/**" = true;
      "**/.hg/store/**" = true;
      "**/target/debug/**" = true;
      "**/.mypy_cache/**" = true;
      "**/bazel-*/**" = true;
    };
    "explorer.confirmDragAndDrop" = false;
    "explorer.decorations.badges" = false;
    "explorer.decorations.colors" = true;
    "explorer.incrementalNaming" = "smart";
    "explorer.openEditors.visible" = 8;
    "search.smartCase" = true;

    # Terminal
    "terminal.external.linuxExec" = config.modules.desktop.term.default;
    "terminal.integrated.rendererType" = "experimentalWebgl";
    "terminal.integrated.fontSize" = 22;

    # Language specific
    "scm.defaultViewMode" = "tree";
    "update.mode" = "none";
    "html.format.contentUnformatted" = "pre,code,style,textarea";
    "html.format.extraLiners" = "";
    "html.format.wrapLineLength" = 0;
    "php.validate.run" = "onType";
    "javascript.format.insertSpaceAfterConstructor" = true;
    "javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" = true;
    "javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets" = true;
    "javascript.format.insertSpaceBeforeFunctionParenthesis" = true;
    "javascript.referencesCodeLens.enabled" = true;
    "typescript.format.insertSpaceAfterConstructor" = true;
    "typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" = true;
    "typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets" = true;
    "typescript.format.insertSpaceBeforeFunctionParenthesis" = true;
    "typescript.implementationsCodeLens.enabled" = true;
    "typescript.referencesCodeLens.enabled" = true;
    "typescript.tsc.autoDetect" = "watch";
    "extensions.autoUpdate" = false;
    "git.autofetch" = true;
    "git.allowForcePush" = true;
    "git.confirmSync" = false;
    "git.detectSubmodules" = false;
    "git.enableCommitSigning" = true;
    "git.enableSmartCommit" = true;
    "git.showPushSuccessNotification" = true;
    "C_Cpp.clang_format_path" = "${llvmPackages.clang-unwrapped}/bin/clang-format";
    "C_Cpp.clang_format_fallbackStyle" = "LLVM";
    # "C_Cpp.formatting" = "Disabled";
    "C_Cpp.intelliSenseEngineFallback" = "Enabled";
    "C_Cpp.workspaceParsingPriority" = "medium";
    "clang.executable" = "${clang}/bin/clang";
    "cmake.cmakePath" = "${cmake}/bin/cmake";
    "emmet.includeLanguages" = {
      "jinja-html" = "html";
    };
    "eslint.nodePath" = "${nodePackages.eslint}/lib/node_modules";
    "eslint.packageManager" = "yarn";
    "eslint.validate" = [
      "html"
      "javascript"
      "javascriptreact"
    ];
    "gitlens.codeLens.scopes" = [
      "document"
      "containers"
      "blocks"
    ];
    # "java.home" = JAVA_HOME;
    "java.implementationsCodeLens.enabled" = true;
    "java.referencesCodeLens.enabled" = true;
    "java.saveActions.organizeImports" = true;
    "jupyter.disableJupyterAutoStart" = true;
    "jupyter.searchForJupyter" = false;
    "jupyter.useNotebookEditor" = false;
    "gopls" = {
      "expandWorkspaceToModule" = true;
      "experimentalWorkspaceModule" = true;
    };
    # "haskell.formattingProvider" = "brittany";
    # "haskell.serverExecutablePath" = "\${workspaceFolder}/hie-wrapper.sh";
    "latex-workshop.linting.chktex.enabled" = true;
    "latex-workshop.linting.chktex.exec.path" = "${texlive.combined.scheme-full}/bin/chktex";
    "latex-workshop.latex.autoClean.run" = "onBuilt";
    "latex-workshop.latex.tools" = [
      {
        "name" = "latexmk";
        "command" = "latexmk";
        "args" = [
          "-f"
          "-cd"
          "-pdf"
          "-file-line-error"
          "-synctex=1"
          "-interaction=nonstopmode"
          "-outdir=./"
          "-xelatex"
          "%DOC%"
        ];
      }

      {
        "name" = "pdflatex";
        "command" = "pdflatex";
        "args" = [
          "-synctex=1"
          "-interaction=nonstopmode"
          "-file-line-error"
          "%DOC%"
        ];
      }

      {
        "name" = "bibtex";
        "command" = "bibtex";
        "args" = [
          "%DOCFILE%"
        ];
      }
    ];
    "latex-workshop.latexindent.path" = "${texlive.combined.scheme-full}/bin/latexindent";
    "latex-workshop.synctex.path" = "${texlive.combined.scheme-full}/bin/synctex";
    "latex-workshop.texdoc.path" = "${texlive.combined.scheme-full}/bin/texdoc";
    "latex-workshop.view.pdf.viewer" = "tab";
    "materialTheme.accent" = "Graphite";
    "npm.packageManager" = "yarn";
    "path-intellisense.showHiddenFiles" = true;
    "python.analysis.completeFunctionParens" = true;
    "python.autoComplete.addBrackets" = true;
    "python.autoUpdateLanguageServer" = false;
    "python.defaultInterpreterPath" = "${python3Packages.python}/bin/python3";
    "python.formatting.provider" = "yapf";
    "python.formatting.yapfPath" = "${python3Packages.yapf}/bin/yapf";
    "python.languageServer" = "Pylance";
    "python.linting.flake8Enabled" = true;
    "python.linting.flake8Path" = "${python3Packages.flake8}/bin/flake8";
    "python.linting.mypyEnabled" = true;
    "python.linting.mypyPath" = "${python3Packages.mypy}/bin/mypy";
    "python.linting.pycodestyleEnabled" = true;
    "python.linting.pycodestylePath" = "${python3Packages.pycodestyle}/bin/pycodestyle";
    "python.linting.pydocstyleEnabled" = false;
    "python.linting.pydocstylePath" = "${python3Packages.pydocstyle}/bin/pydocstyle";
    "python.linting.pylamaEnabled" = true;
    "python.linting.pylamaPath" = "${python3Packages.pylama}/bin/pylama";
    "python.linting.pylintPath" = "${python3Packages.pylint}/bin/pylint";
    "python.testing.unittestEnabled" = true;
    "maven.executable.path" = "${maven}/bin/mvn";
    "maven.terminal.useJavaHome" = true;
    "rust-analyzer.server.path" = "${rust-analyzer}/bin/rust-analyzer";
    "sqlite.sqlite3" = "${sqlite}/bin/sqlite3";
    "todo-tree.general.tags" = ["BUG" "HACK" "FIXME" "TODO" "XXX" "[ ]" "[x]"];
    "todo-tree.tree.showScanModeButton" = false;
    "tslint.packageManager" = "yarn";
    "tslint.validateWithDefaultConfig" = true;

    "pdf-preview.default.scrollMode" = "wrapped";
    "pdf-preview.default.scale" = "page-fit";

    "workbench.colorTheme" = "Min Tomorrow Dark";
    # Fix diff colors for "Min Tomorrow Dark" theme
    "editor.tokenColorCustomizations" = {
      "textMateRules" = [
        {
          "name" = "markup diff";
          "scope" = "markup.changed.diff";
          "settings" = {
            "foreground" = "#f0c674";
          };
        }
        {
          "name" = "diff";
          "scope" = "meta.diff.header.from-filemeta.diff.header.to-file,punctuation.definition.from-file.diff,punctuation.definition.to-file.diff";
          "settings" = {
            "foreground" = "#81a2be";
          };
        }
        {
          "name" = "inserted.diff";
          "scope" = "markup.inserted.diff";
          "settings" = {
            "foreground" = "#b5bd68";
          };
        }
        {
          "name" = "deleted.diff";
          "scope" = "markup.deleted.diff";
          "settings" = {
            "foreground" = "#cc6666";
          };
        }
      ];
    };

    "markdown.preview.typographer" = true;

    "[markdown]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[css]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[html]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
  }
