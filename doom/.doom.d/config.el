;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq custom-file (concat doom-etc-dir "custom.el"))

(setq doom-theme 'doom-tomorrow-night)

(setq doom-font     (font-spec :family "Iosevka" :size 12)
      doom-big-font (font-spec :family "Iosevka" :size 19))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(setq org-log-into-drawer "LOGBOOK")

(setq-default c-default-style "linux")

(setq haskell-process-type 'cabal-new-repl)


