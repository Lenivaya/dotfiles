{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with pkgs;
let
  inherit (config) modules;
in
mkMerge [
  {
    vim = {
      easymotion = true;
      incsearch = true;
      useSystemClipboard = true;
      useCtrlKeys = true;
      hlsearch = true;
      sneak = true;
      camelCaseMotion.enable = true;

      insertModeKeyBindings = [
        {
          before = [
            "j"
            "j"
          ];
          after = [ "<Esc>" ];
        }
      ];
      normalModeKeyBindingsNonRecursive = [
        {
          before = [ "<Esc>" ];
          commands = [ ":nohl" ];
        }
        {
          before = [ "<space>" ];
          commands = [ "vspacecode.space" ];
        }
        {
          before = [ "," ];
          commands = [
            "vspacecode.space"
            {
              command = "whichkey.triggerKey";
              args = "m";
            }
          ];
        }
        {
          "before" = [
            "g"
            "r"
          ];
          "commands" = [ "editor.action.rename" ];
        }
      ];
      visualModeKeyBindingsNonRecursive = [
        {
          before = [ "<space>" ];
          commands = [ "vspacecode.show" ];
        }
        {
          before = [ "," ];
          commands = [
            "vspacecode.space"
            {
              command = "whichkey.triggerKey";
              args = "m";
            }
          ];
        }
      ];

      autoSwitchInputMethod =
        let
          switcher = getExe xkb-switch;
        in
        {
          enable = true;
          defaultIM = "us";
          obtainIMCmd = switcher;
          switchIMCmd = "${switcher} -s {im}";
        };
    };

    # Editor
    editor.mouseWheelZoom = true;
    editor.cursorBlinking = "smooth";
    editor.cursorSmoothCaretAnimation = true;
    editor.fontLigatures = true;
    editor.lineHeight = 20;
    editor.lineNumbers = "relative";
    editor.multiCursorModifier = "ctrlCmd";
    editor.quickSuggestions = {
      other = true;
      comments = true;
      strings = true;
    };
    editor.renderIndentGuides = false;
    editor.renderWhitespace = "none";
    editor.rulers = [
      80
      100
    ];
    editor.smoothScrolling = true;
    editor.snippetSuggestions = "top";
    editor.suggest.localityBonus = true;
    editor.tabSize = 2;
    editor.wordWrap = "off";
    editor.minimap.showSlider = "always";
    editor.inlayHints.enabled = "offUnlessPressed";
    "editor.quickSuggestions" = {
      comments = "on";
      strings = "on";
    };
    "editor.wordBasedSuggestions" = true;
    "editor.wordBasedSuggestionsMode" = "allDocuments";
    diffEditor.ignoreTrimWhitespace = false;

    workbench.colorCustomizations = {
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
    workbench.editorAssociations = [
      {
        viewType = "jupyter-notebook";
        filenamePattern = "*.ipynb";
      }
    ];
    workbench.list.smoothScrolling = true;
    workbench.iconTheme = "vs-seti";
    workbench.sideBar.location = "right";
    workbench.tree.expandMode = "singleClick";
    workbench.startupEditor = "None";
    breadcrumbs.enabled = true;
    window.menuBarVisibility = "toggle";
    window.titleBarStyle = "custom"; # "native" without command center
    files.associations = {
      "bspwmrc" = "shellscript";
      "sxhkdrc" = "shellscript";
      "*.pug" = "jade";
      "*.toml" = "ini";
      "*.conkyrc" = "lua";
      "*.xmobarrc" = "haskell";
      ".Xresources" = "shellscript";
    };
    files.enableTrash = false;
    files.exclude = {
      "**/.git" = false;
      "**/.svn" = true;
      "**/.hg" = true;
      "**/CVS" = true;
      "**/.DS_Store" = false;
      "**/*.olean" = true;
    };
    files.insertFinalNewline = true;
    files.trimTrailingWhitespace = true;
    files.watcherExclude = {
      "**/.git/objects/**" = true;
      "**/.git/subtree-cache/**" = true;
      "**/node_modules/**" = true;
      "**/.hg/store/**" = true;
      "**/target/debug/**" = true;
      "**/.mypy_cache/**" = true;
      "**/bazel-*/**" = true;
    };
    files.refactoring.autoSave = true;
    explorer.confirmDragAndDrop = false;
    explorer.decorations.badges = false;
    explorer.decorations.colors = true;
    explorer.incrementalNaming = "smart";
    explorer.openEditors.visible = 8;
    "explorer.autoReveal" = false;
    search.smartCase = true;

    # Terminal
    terminal.external.linuxExec = config.modules.desktop.term.default;
    terminal.integrated.rendererType = "experimentalWebgl";
    terminal.integrated.fontSize = 22;

    # Language specific
    scm.defaultViewMode = "tree";
    update.mode = "none";
    html.format.contentUnformatted = "pre,code,style,textarea";
    html.format.extraLiners = "";
    html.format.wrapLineLength = 0;
    php.validate.run = "onType";
    javascript.format.insertSpaceAfterConstructor = true;
    javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true;
    javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets = true;
    javascript.format.insertSpaceBeforeFunctionParenthesis = true;
    javascript.referencesCodeLens.enabled = true;
    typescript.format.insertSpaceAfterConstructor = true;
    typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true;
    typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets = true;
    typescript.format.insertSpaceBeforeFunctionParenthesis = true;
    typescript.implementationsCodeLens.enabled = true;
    typescript.referencesCodeLens.enabled = true;
    typescript.tsc.autoDetect = "watch";
    extensions.autoUpdate = false;

    git = {
      autofetch = true;
      allowForcePush = true;
      confirmSync = false;
      detectSubmodules = false;
      enableCommitSigning = true;
      enableSmartCommit = true;
      showPushSuccessNotification = true;
    };
    explorer.excludeGitIgnore = true;

    C_Cpp.clang_format_fallbackStyle = "LLVM";
    # C_Cpp.formatting =  "Disabled";
    C_Cpp.intelliSenseEngineFallback = "Enabled";
    C_Cpp.workspaceParsingPriority = "medium";
    emmet.includeLanguages = {
      jinja-html = "html";
    };
    eslint.packageManager = "yarn";
    eslint.validate = [
      "html"
      "javascript"
      "javascriptreact"
    ];
    gitlens.codeLens.scopes = [
      "document"
      "containers"
      "blocks"
    ];
    # java.home =  JAVA_HOME;
    java.implementationsCodeLens.enabled = true;
    java.referencesCodeLens.enabled = true;
    java.saveActions.organizeImports = true;
    jupyter.disableJupyterAutoStart = true;
    jupyter.searchForJupyter = false;
    jupyter.useNotebookEditor = false;
    gopls = {
      expandWorkspaceToModule = true;
      experimentalWorkspaceModule = true;
    };

    # haskell.formattingProvider =  "brittany";
    # haskell.serverExecutablePath =  "\${workspaceFolder}/hie-wrapper.sh";
    haskell = {
      formattingProvider = "fourmolu";
      # serverExecutablePath = "${pkgs.haskell-language-server}/bin/haskell-language-server";
    };

    latex-workshop.linting.chktex.enabled = true;
    # latex-workshop.linting.chktex.exec.path = "${texlive.combined.scheme-full}/bin/chktex";
    latex-workshop.latex.autoClean.run = "onBuilt";
    latex-workshop.latex.tools = [
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
        "args" = [ "%DOCFILE%" ];
      }
    ];
    # latex-workshop.latexindent.path = "${texlive.combined.scheme-full}/bin/latexindent";
    # latex-workshop.synctex.path = "${texlive.combined.scheme-full}/bin/synctex";
    # latex-workshop.texdoc.path = "${texlive.combined.scheme-full}/bin/texdoc";
    latex-workshop.view.pdf.viewer = "tab";

    materialTheme.accent = "Graphite";
    npm.packageManager = "yarn";
    path-intellisense.showHiddenFiles = true;

    nix = {
      enableLanguageServer = true;
      serverPath = getExe nil;
    };

    # maven.executable.path = "${maven}/bin/mvn";
    maven.terminal.useJavaHome = true;
    # rust-analyzer.server.path = getExe rust-analyzer;
    # sqlite.sqlite3 = getExe sqlite;
    todo-tree.general.tags = [
      "BUG"
      "HACK"
      "FIXME"
      "TODO"
      "XXX"
      "[ ]"
      "[x]"
    ];
    todo-tree.tree.showScanModeButton = false;
    tslint.packageManager = "yarn";
    tslint.validateWithDefaultConfig = true;

    pdf-preview.default.scrollMode = "wrapped";
    pdf-preview.default.scale = "page-fit";

    workbench.colorTheme = "Min Tomorrow Dark";
    # Fix diff colors for "Min Tomorrow Dark" theme
    editor.tokenColorCustomizations = {
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

    markdown.preview.typographer = true;

    "codesnap.showLineNumbers" = false;

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

  (mkIf modules.dev.python.enable {
    python = with python3Packages; {
      experiments.optOutFrom = [ "All" ];
      analysis.completeFunctionParens = true;
      autoComplete.addBrackets = true;
      autoUpdateLanguageServer = false;
      defaultInterpreterPath = getExe python3;
      formatting.provider = "yapf";
      formatting.yapfPath = getExe yapf;
      languageServer = "Pylance";
      linting = {
        flake8Enabled = true;
        flake8Path = getExe flake8;
        mypyEnabled = true;
        banditPath = getExe bandit;
        mypyPath = getExe mypy;
        pycodestyleEnabled = true;
        pycodestylePath = getExe pycodestyle;
        pydocstyleEnabled = false;
        pydocstylePath = getExe pydocstyle;
        pylamaEnabled = true;
        pylamaPath = getExe pylama;
        pylintPath = getExe pylint;
      };
      testing.unittestEnabled = true;
    };
  })
]
