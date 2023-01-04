# Place your key bindings in this file to overwrite the defaults
[
  {
    "key" = "ctrl+p";
    "command" = "cursorUp";
    "when" = "textInputFocus";
  }

  {
    "key" = "ctrl+n";
    "command" = "cursorDown";
    "when" = "textInputFocus";
  }

  {
    "key" = "ctrl+p";
    "command" = "showPrevParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "up";
    "command" = "-showPrevParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "alt+up";
    "command" = "-showPrevParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "ctrl+n";
    "command" = "showNextParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "down";
    "command" = "-showNextParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "alt+down";
    "command" = "-showNextParameterHint";
    "when" = "editorTextFocus && parameterHintsVisible";
  }

  {
    "key" = "ctrl+p";
    "command" = "selectPrevQuickFix";
    "when" = "editorFocus && quickFixWidgetVisible";
  }

  {
    "key" = "ctrl+n";
    "command" = "selectNextQuickFix";
    "when" = "editorFocus && quickFixWidgetVisible";
  }

  {
    "key" = "ctrl+p";
    "command" = "selectPrevSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "up";
    "command" = "-selectPrevSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "ctrl+up";
    "command" = "-selectPrevSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "ctrl+n";
    "command" = "selectNextSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "down";
    "command" = "-selectNextSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "ctrl+down";
    "command" = "-selectNextSuggestion";
    "when" = "editorTextFocus && suggestWidgetVisible";
  }

  {
    "key" = "ctrl+p";
    "command" = "workbench.action.quickOpenNavigatePrevious";
    "when" = "inQuickOpen";
  }

  {
    "key" = "ctrl+n";
    "command" = "workbench.action.quickOpenNavigateNext";
    "when" = "inQuickOpen";
  }

  {
    "key" = "alt+x";
    "command" = "editor.action.toggleWordWrap";
  }

  {
    "key" = "alt+z";
    "command" = "-editor.action.toggleWordWrap";
  }

  {
    "key" = "ctrl+x d";
    "command" = "workbench.action.files.openFolder";
  }

  {
    "key" = "ctrl+k ctrl+o";
    "command" = "-workbench.action.files.openFolder";
  }

  {
    "key" = "ctrl+x ctrl+s";
    "command" = "workbench.action.files.save";
    "when" = "editorTextFocus";
  }

  {
    "key" = "ctrl+s";
    "command" = "-workbench.action.files.save";
    "when" = "editorTextFocus";
  }

  {
    "key" = "ctrl+x s";
    "command" = "workbench.action.files.saveAll";
    "when" = "editorTextFocus";
  }

  {
    "key" = "ctrl+shift+s";
    "command" = "-workbench.action.files.saveAll";
    "when" = "editorTextFocus";
  }

  {
    "key" = "ctrl+x 2";
    "command" = "workbench.action.splitEditor";
  }

  {
    "key" = "ctrl+\\";
    "command" = "-workbench.action.splitEditor";
  }

  {
    "key" = "ctrl+x 3";
    "command" = "workbench.action.splitEditorOrthogonal";
  }

  {
    "key" = "ctrl+k ctrl+\\";
    "command" = "-workbench.action.splitEditorOrthogonal";
  }

  {
    "key" = "ctrl+x k";
    "command" = "workbench.action.closeActiveEditor";
  }

  {
    "key" = "ctrl+f4";
    "command" = "-workbench.action.closeActiveEditor";
  }

  {
    "key" = "ctrl+w";
    "command" = "workbench.action.closeActiveEditor";
  }

  {
    "key" = "ctrl+x ctrl+c";
    "command" = "workbench.action.quit";
  }

  # magit + vim
  {
    "key" = "g g";
    "command" = "cursorTop";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "g r";
    "command" = "magit.refresh";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "tab";
    "command" = "extension.vim_tab";
    "when" = "editorFocus && vim.active && !inDebugRepl && vim.mode != 'Insert' && editorLangId != 'magit'";
  }
  {
    "key" = "tab";
    "command" = "-extension.vim_tab";
    "when" = "editorFocus && vim.active && !inDebugRepl && vim.mode != 'Insert'";
  }
  {
    "key" = "x";
    "command" = "magit.discard-at-point";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "k";
    "command" = "-magit.discard-at-point";
  }
  {
    "key" = "-";
    "command" = "magit.reverse-at-point";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "v";
    "command" = "-magit.reverse-at-point";
  }
  {
    "key" = "shift+-";
    "command" = "magit.reverting";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "shift+v";
    "command" = "-magit.reverting";
  }
  {
    "key" = "shift+o";
    "command" = "magit.resetting";
    "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    "key" = "shift+x";
    "command" = "-magit.resetting";
  }
  {
    "key" = "x";
    "command" = "-magit.reset-mixed";
  }
  {
    "key" = "ctrl+u x";
    "command" = "-magit.reset-hard";
  }

  # Toggle sidebar visibillity
  {
    "key" = "ctrl+e";
    "command" = "workbench.view.explorer";
  }
  {
    "key" = "ctrl+e";
    "command" = "-workbench.view.explorer";
  }
  {
    "key" = "ctrl+e";
    "command" = "workbench.action.toggleSidebarVisibility";
    "when" = "explorerViewletVisible";
  }
  {
    "key" = "ctrl+e";
    "command" = "-workbench.action.toggleSidebarVisibility";
    "when" = "explorerViewletVisible";
  }

  # Zoom only current editor with default keybindings
  {
    "key" = "ctrl+=";
    "command" = "editor.action.fontZoomIn";
  }

  {
    "key" = "ctrl+-";
    "command" = "editor.action.fontZoomOut";
  }

  # And zoom vscode with extra hotkeys
  {
    "key" = "ctrl+alt+=";
    "command" = "workbench.action.zoomIn";
  }

  {
    "key" = "ctrl+alt+-";
    "command" = "workbench.action.zoomOut";
  }

  # Reset both, editor and workbench font size's
  {
    "key" = "ctrl+shift+=";
    "command" = "extension.multiCommand.execute";
    "args" = {
      "sequence" = [
        "editor.action.fontZoomReset"
        "workbench.action.zoomReset"
      ];
    };
  }

  # DISABLING
  {
    "key" = "ctrl+=";
    "command" = "-workbench.action.zoomIn";
  }

  {
    "key" = "ctrl+-";
    "command" = "-workbench.action.zoomOut";
  }

  {
    "key" = "ctrl+shift+=";
    "command" = "-workbench.action.zoomReset";
  }

  {
    "key" = "space";
    "command" = "-list.toggleExpand";
    "when" = "listFocus";
  }

  {
    "key" = "ctrl+D";
    "command" = "-editor.action.copyLinesDownAction";
  }

  {
    "key" = "ctrl+shift+g";
    "command" = "-workbench.action.files.openFileFolder";
  }
]
