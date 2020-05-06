;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Daniil Osipchuk"
      user-mail-address "xocada@gmail.com"


      doom-theme 'doom-tomorrow-night
      treemacs-width 32)

;;
;;; UI
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;; Pragmata ligatures
;; (load! "pragmata-lig.el")


;;
;;; Modules

;; I prefer search matching to be ordered; it's more precise
(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Silence all that useless output
(setq direnv-always-show-summary nil)

;; Latex preview
(setq +latex-viewers '(pdf-tools))


;;
;;; Packages
(use-package! reverse-im
  :config
  (reverse-im-activate '("russian-computer" "ukrainian-computer")))

(use-package! kdeconnect
  :config
  (setq kdeconnect-devices "274bdfc779b2e09e"))
