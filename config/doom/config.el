;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Daniil Osipchuk"
      user-mail-address "xocada@gmail.com"

      doom-theme 'doom-tomorrow-night
      treemacs-width 32)

(setq doom-font (font-spec :family "monospace" :size 12 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Fix incorrect color of guides
(after! highlight-indent-guides
  (highlight-indent-guides-auto-set-faces))

(use-package! reverse-im
  :config
  (reverse-im-activate '("russian-computer" "ukrainian-computer")))

;; Pragmata ligatures
(load! "pragmata-lig.el")
