;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq custom-file (concat doom-etc-dir "custom.el"))

(setq doom-theme 'doom-tomorrow-night)

(setq doom-font     (font-spec :family "Iosevka" :size 12)
      doom-big-font (font-spec :family "Iosevka" :size 19))

;;: Centaur tabs
; (require 'centaur-tabs)
; (centaur-tabs-mode t)
; (global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
; (global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
; (setq centaur-tabs-style "bar"
;         centaur-tabs-set-icons t
;       centaur-tabs-gray-out-icons t
;       centaur-tabs-set-bar 'left
;       centaur-tabs-set-bar 'under
;       centaur-tabs-set-modified-marker t)
; (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
; (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)
; (defun centaur-tabs-hide-tab (x)
;    (let ((name (format "%s" x)))
;        (or
;         (string-prefix-p "*epc" name)
;         (string-prefix-p "*helm" name)
;         (string-prefix-p "*Compile-Log*" name)
;         (string-prefix-p "*lsp" name)
;         (string-prefix-p "*doom" name)
;         (and (string-prefix-p "magit" name)
;         (not (file-name-extension name)))
;                  )))
;
;
