;;; General

(setq user-full-name "Daniil Osipchuk"
      user-mail-address "xocada@gmail.com")

;;
;;; UI
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; take new window space from all other windows (not just current)
(setq window-combination-resize t)

;; Don't blink the cursor, it's too distracting.
(setq visible-cursor nil
      blink-cursor-mode -1)

;; Don't like symbols, but want font ligatures
(setq +pretty-code-symbols nil)

;; I don't need it to tell me its UTF-8
(setq doom-modeline-buffer-encoding nil
      +modeline-encoding nil)


;;
;;; Modules

;; Avy
(setq
 ;; Avy can jump through windows
 avy-all-windows t
 ;; Avy can auto-jump when theres 1 candidate
 avy-single-candidate-jump t)

;; :completions ivy
(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; :tools direnv
;; Silence all that useless output
(setq direnv-always-show-summary nil)

;; Latex preview
(setq +latex-viewers '(zathura, pdf-tools))

;; :lang org
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " ▼ "
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
      org-export-in-background t)

;; Treemacs
(setq
 treemacs-width 32)


;;
;;; Packages
(use-package! reverse-im
  :config
  (reverse-im-activate '("russian-computer" "ukrainian-computer")))

(use-package! kdeconnect
  :config
  (setq kdeconnect-devices "c714076c998c5de4"))

(use-package! which-key-posframe
  :config
  (setq
   which-key-posframe-border-width 0
   which-key-posframe-poshandler #'posframe-poshandler-frame-center)
  (which-key-posframe-mode 1))

(use-package! deadgrep
  :defer t
  :init
  (map!
   (:leader
    :desc "Search via deadgrep" "s <f5>" #'deadgrep))
  )

(use-package! imenu-list
  :defer t
  :init
  (setq
   ;; just a tad lower than the default
   imenu-list-size 0.25
   ;; That modeline is plain ugly. Treemacs & neotree don't have a modeline either.
   imenu-list-mode-line-format nil)
  (map!
   (:leader
    :desc "Toggle imenu-list" "oi" #'imenu-list-smart-toggle)
   :map imenu-list-major-mode-map
   :g "r"   #'imenu-list-refresh
   :g [tab] #'hs-toggle-hiding
   :n "gr"  #'imenu-list-refresh))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-save-place-file (concat doom-etc-dir "nov-places")))

(use-package company-fuzzy
  :defer t
  :init
  (setq company-fuzzy-sorting-backend 'flx)
  (setq company-fuzzy-prefix-ontop nil)
  (with-eval-after-load 'company
    (global-company-fuzzy-mode t)))

(use-package! circadian
  :config
  (setq circadian-themes '(("8:00" . doom-solarized-light)
                           ("19:30" . doom-solarized-dark)))
  (circadian-setup))

(use-package! emojify
  :defer t
  :config
  (setq! emojify-emoji-styles '(unicode github))
  (global-emojify-mode))
