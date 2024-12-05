;;; config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Personal information][Personal information:1]]
(setq user-full-name "Danylo Osipchuk"
      user-mail-address "xocada@gmail.com")
;; Personal information:1 ends here

;; [[file:config.org::*Simple settings][Simple settings:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; Simple settings:1 ends here

;; [[file:config.org::*Frame sizing][Frame sizing:1]]
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
;; Frame sizing:1 ends here

;; [[file:config.org::*Auto-customisations][Auto-customisations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Auto-customisations:1 ends here

;; [[file:config.org::*Windows][Windows:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Windows:1 ends here

;; [[file:config.org::*Windows][Windows:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
;; Windows:2 ends here

;; [[file:config.org::*Windows][Windows:3]]
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)
;; Windows:3 ends here

;; [[file:config.org::*Font Face][Font Face:1]]
;; (setq! doom-font (font-spec :family "monospace" :size 16)
;;        ;; doom-big-font (font-spec :family "monospace" :size 26)
;;        doom-variable-pitch-font (font-spec :family "sans" :size 16)
;;        ;; doom-unicode-font (font-spec :family "JuliaMono")
;;        doom-unicode-font doom-variable-pitch-font
;;        ;; doom-unicode-font doom-font
;;        doom-serif-font doom-variable-pitch-font
;;        )

(setq doom-font (font-spec :family "monospace" :size 16)
      doom-big-font (font-spec :family "monospace" :size 26)
      doom-variable-pitch-font (font-spec :family "sans" :size 16)
      ;; doom-unicode-font (font-spec :family "JuliaMono")
      doom-unicode-font doom-variable-pitch-font
      doom-serif-font doom-variable-pitch-font)

;; Fix cyryllic fonts
;; (if (display-graphic-p)

;;     (dolist (charset '(koi8))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset (font-spec :family "sans" :height 90)))

;;   (after! unicode-fonts
;;     (dolist (unicode-block '("Cyrillic"
;;                              "Cyrillic Supplement"))
;;       (push "sans" (cadr (assoc unicode-block unicode-fonts-block-font-mapping)))))

;;   )
;; Font Face:1 ends here

;; [[file:config.org::*Theme and modeline][Theme and modeline:1]]
(setq doom-theme 'doom-tomorrow-night)
;; Theme and modeline:1 ends here

;; [[file:config.org::*Theme and modeline][Theme and modeline:2]]
;; I don't need it to tell me its UTF-8
(setq doom-modeline-buffer-encoding nil
      +modeline-encoding nil)
;; Theme and modeline:2 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:1]]
(setq display-line-numbers-type 'relative)
;; Miscellaneous:1 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:2]]
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; Miscellaneous:2 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:3]]
(setq visible-cursor nil
      blink-cursor-mode -1)
;; Miscellaneous:3 ends here

;; [[file:config.org::*Ligatures][Ligatures:1]]
(plist-put! +ligatures-extra-symbols
  :int           nil
  :name          nil
  :composition   nil
  :map           nil
  :not           nil
  :and           nil
  :or            nil
  :for           nil
  :return        nil
  :yield         nil
  :diff          nil
  :tuple         nil
  :pipe          nil
)
;; Ligatures:1 ends here

;; [[file:config.org::*Performance][Performance:1]]
(setq gcmh-high-cons-threshold most-positive-fixnum)
(setq max-specpdl-size 100000)
;; Performance:1 ends here

;; [[file:config.org::*Mouse buttons][Mouse buttons:1]]
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
;; Mouse buttons:1 ends here

;; [[file:config.org::*Splash screen][Splash screen:1]]
(setq fancy-splash-image (concat doom-user-dir "splash.png"))
;; Splash screen:1 ends here

;; [[file:config.org::*Splash screen][Splash screen:2]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
;; Splash screen:2 ends here

;; [[file:config.org::daemon initialization][daemon initialization]]
(when (daemonp)
  (add-hook! 'server-after-make-frame-hook (switch-to-buffer +doom-dashboard-name)))
;; daemon initialization ends here

;; [[file:config.org::*Automatically save sessions][Automatically save sessions:1]]
;; Set up automatic session saving every hour (3600 seconds)
(run-with-timer 3600 3600 #'doom/quicksave-session)
;; Automatically save sessions:1 ends here

(use-package! aas
  :commands aas-mode)

(use-package! laas
  :hook ((org-mode . laas-mode)
         (org-mode . aas-activate-for-major-mode)
         (LaTeX-mode . laas-mode))
  :config
  (defun laas-tex-fold-maybe ()
    (unless (equal "/" aas-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! ox-gfm
  :after org)

(use-package! org-pandoc-import
  :after org)

(use-package! authinfo-color-mode
  :mode ("authinfo.gpg\\'" . authinfo-color-mode)
  :init (advice-add 'authinfo-mode :override #'authinfo-color-mode))

;; [[file:config.org::*Abbrev mode][Abbrev mode:1]]
(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-user-dir))
  (setq save-abbrevs 'silently))
;; Abbrev mode:1 ends here

;; [[file:config.org::*Avy][Avy:1]]
(setq
 ;; Avy can jump through windows
 avy-all-windows t
 ;; Avy can auto-jump when theres 1 candidate
 avy-single-candidate-jump t)
;; Avy:1 ends here

;; [[file:config.org::*Calc][Calc:1]]
(add-hook 'calc-mode-hook #'calctex-mode)
;; Calc:1 ends here

;; [[file:config.org::*Deadgrep][Deadgrep:1]]
(use-package! deadgrep
  :defer t
  :init
  (map!
   (:leader
    :desc "Search via deadgrep" "s <f5>" #'deadgrep))
  )
;; Deadgrep:1 ends here

;; [[file:config.org::*Direnv][Direnv:1]]
(setq direnv-always-show-summary nil)
;; Direnv:1 ends here

;; [[file:config.org::*Height][Height:1]]
(setq doom-modeline-height 45)
;; Height:1 ends here

;; [[file:config.org::*Emacs-everywhere][Emacs-everywhere:1]]
(after! emacs-everywhere
  ;; Make it easier to match with xmonad rule
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)
  (remove-hook! 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)
  )
;; Emacs-everywhere:1 ends here

;; [[file:config.org::*Eros-eval][Eros-eval:1]]
(setq eros-eval-result-prefix "⟹ ")
;; Eros-eval:1 ends here

;; [[file:config.org::*EVIL][EVIL:1]]
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring
;; EVIL:1 ends here

;; [[file:config.org::*Gif-screencast][Gif-screencast:1]]
(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))
;; Gif-screencast:1 ends here

;; [[file:config.org::*Google-translate][Google-translate:1]]
;; (use-package! go-translate
;;   :init
;;   (setq
;;    gt-langs '(en ua ru)
;;    gt-default-translator (gt-translator :engines (gt-google-engine))
;;    )
;;   (map!
;;    (:leader
;;     (:prefix-map ("o g" . "google")
;;      :desc "Open google-translator" "t" #'gt-do-translate)
;;     ))
;;   )
;; Google-translate:1 ends here

;; [[file:config.org::*Systemd][Systemd:2]]
(use-package! systemd
  :defer t)
;; Systemd:2 ends here

;; [[file:config.org::*Imenu][Imenu:1]]
(use-package! imenu-list
  :init
  (setq
   imenu-list-position 'right
   ;; just a tad lower than the default
   imenu-list-size 0.25
   ;; That modeline is plain ugly. Treemacs & neotree don't have a modeline either.
   imenu-list-mode-line-format nil
   ;; imenu-list-auto-resize t
   imenu-list-auto-resize nil
   )
  :config
  (map!
   (:leader
    :desc "Toggle imenu-list" "oi" #'imenu-list-smart-toggle
    )
   :map imenu-list-major-mode-map
   :g "r"   #'imenu-list-refresh
   :g [tab] #'hs-toggle-hiding
   :n "gr"  #'imenu-list-refresh))
;; Imenu:1 ends here

;; [[file:config.org::*Info colours][Info colours:1]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)
;; Info colours:1 ends here

;; [[file:config.org::*Leetcode][Leetcode:2]]
;; (use-package! leetcode
;;   :config
;;   (setq leetcode-save-solutions t
;;         leetcode-directory "~/code/practice/leetcode"
;;         leetcode-prefer-sql "sqlite3")
;;   )
;; Leetcode:2 ends here

;; [[file:config.org::*Kdeconnect][Kdeconnect:1]]
(use-package! kdeconnect
  :config
  (setq kdeconnect-devices "2580ec370a27fbb7"))
;; Kdeconnect:1 ends here

;; [[file:config.org::*Keycast][Keycast:1]]
(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold)))
;; Keycast:1 ends here

;; [[file:config.org::*Org Chef][Org Chef:1]]
(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))
;; Org Chef:1 ends here

;; [[file:config.org::*Reverse-im (Keyboard layouts)][Reverse-im (Keyboard layouts):1]]
;; Needed for `:after char-fold' to work
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package! reverse-im
  :ensure t
  :demand t
  :after char-fold ; but only after `char-fold' is loaded
  :bind
  ("M-T" . reverse-im-translate-word)
  :custom
  (reverse-im-avy-action-char nil)
  (reverse-im-input-methods '("russian-computer" "ukrainian-computer"))
  ;; That breaks avy
  ;; (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (reverse-im-char-fold t)
  :config
  (reverse-im-mode t)
  )
;; Reverse-im (Keyboard layouts):1 ends here

;; [[file:config.org::*Which-key][Which-key:1]]
(setq which-key-idle-delay 0.3) ;; I need the help, I really do
;; Which-key:1 ends here

;; [[file:config.org::*Which-key][Which-key:2]]
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; Which-key:2 ends here

;; [[file:config.org::*Writeroom][Writeroom:1]]
(setq +zen-text-scale 1.2)
;; Writeroom:1 ends here

;; [[file:config.org::*Writeroom][Writeroom:2]]
(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide t
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defun +zen-prose-org-h ()
    "Reformat the current Org buffer appearance for prose."
    (when (eq major-mode 'org-mode)
      (setq display-line-numbers nil
            visual-fill-column-width 60
            )
      (when (featurep 'org-modern)
        (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                    ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                    org-modern-hide-stars +zen-org-starhide)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (setq
       +zen--original-org-indent-mode-p org-indent-mode)
      ;; (org-indent-mode -1)
      ))
  (defun +zen-nonprose-org-h ()
    "Reverse the effect of `+zen-prose-org'."
    (when (eq major-mode 'org-mode)
      (when (bound-and-true-p org-modern-mode)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (when +zen--original-org-indent-mode-p (org-indent-mode 1))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h))
;; Writeroom:2 ends here

;; [[file:config.org::*Wakatime][Wakatime:1]]
(use-package! wakatime-mode)
;; Wakatime:1 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(setq yas-triggers-in-field t)
;; YASnippet:1 ends here

;; [[file:config.org::*Fountain][Fountain:2]]
(use-package! fountain-mode)
;; Fountain:2 ends here

;; [[file:config.org::*Open-With][Open-With:2]]
(use-package! crux
  :commands crux-open-with
  :config
  (global-set-key (kbd "C-c o") #'crux-open-with)
  )
;; Open-With:2 ends here

;; [[file:config.org::*Spray][Spray:2]]
(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 600
        spray-height 800)
  (defun spray-mode-hide-cursor ()
    "Hide or unhide the cursor as is appropriate."
    (if spray-mode
        (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
                    evil-normal-state-cursor '(nil))
      (setq-local evil-normal-state-cursor spray--last-evil-cursor-state)))
  (add-hook 'spray-mode-hook #'spray-mode-hide-cursor)
  (map! :map spray-mode-map
        "<return>" #'spray-start/stop
        "f" #'spray-faster
        "s" #'spray-slower
        "t" #'spray-time
        "<right>" #'spray-forward-word
        "h" #'spray-forward-word
        "<left>" #'spray-backward-word
        "l" #'spray-backward-word
        "q" #'spray-quit))
;; Spray:2 ends here

;; [[file:config.org::*iscroll][iscroll:1]]
(use-package! iscroll
  :config
  (add-hook! 'org-mode-hook 'iscroll-mode)
  )
;; iscroll:1 ends here

;; [[file:config.org::*Git blame][Git blame:2]]
(use-package! blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 140
                   :italic t)))
  :init
  (map!
   (:leader
    :desc "Toggle authorship visualizer" "gT"
    (lambda ()
      (interactive)
      (if blamer-mode (blamer-mode 0) (blamer-mode 1))
      )
    ))
  :config
  (blamer-mode 0)
  (global-blamer-mode 0)
  )
;; Git blame:2 ends here

;; [[file:config.org::*Pretty hydra][Pretty hydra:1]]
(use-package! pretty-hydra)
;; Pretty hydra:1 ends here

;; [[file:config.org::*Hydra Posframe][Hydra Posframe:2]]
(use-package! hydra-posframe
  :init
  ;; (setq hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq hydra-posframe-border-width 1)
  (setq hydra-posframe-parameters '((left-fringe . 10)(right-fringe . 10)))
  :config
  (when (display-graphic-p)
    (add-hook! 'after-init-hook 'hydra-posframe-enable)
    )
  )
;; Hydra Posframe:2 ends here

;; [[file:config.org::*Windows][Windows:1]]
(map!
 (:leader
  :desc "Open hydra for windows controls" "w/"
  #'+hydra/window-nav/body
  )
 )
;; Windows:1 ends here

;; [[file:config.org::*Windows][Windows:2]]
;; (defhydra hydra-window (:color blue :hint nil)
;;             "
;;                                                                        ╭─────────┐
;;      Move to      Size    Scroll        Split                    Do    │ Windows │
;;   ╭────────────────────────────────────────────────────────────────────┴─────────╯
;;         ^_k_^           ^_K_^       ^_p_^    ╭─┬─┐^ ^        ╭─┬─┐^ ^         ↺ [_u_] undo layout
;;         ^^↑^^           ^^↑^^       ^^↑^^    │ │ │_v_ertical ├─┼─┤_b_alance   ↻ [_r_] restore layout
;;     _h_ ←   → _l_   _H_ ←   → _L_   ^^ ^^    ╰─┴─╯^ ^        ╰─┴─╯^ ^         ✗ [_d_] close window
;;         ^^↓^^           ^^↓^^       ^^↓^^    ╭───┐^ ^        ╭───┐^ ^         ⇋ [_w_] cycle window
;;         ^_j_^           ^_J_^       ^_n_^    ├───┤_s_tack    │   │_z_oom
;;         ^^ ^^           ^^ ^^       ^^ ^^    ╰───╯^ ^        ╰───╯^ ^
;;   --------------------------------------------------------------------------------
;;             "
;;   ("<tab>" hydra-master/body "back")
;;   ("<ESC>" nil "quit")
;;   ("n" joe-scroll-other-window :color red)
;;   ("p" joe-scroll-other-window-down :color red)
;;   ("b" balance-windows)
;;   ("d" delete-window)
;;   ("H" enlarge-window-horizontally :color red)
;;   ("h" windmove-left :color red)
;;   ("J" shrink-window :color red)
;;   ("j" windmove-down :color red)
;;   ("K" enlarge-window :color red)
;;   ("k" windmove-up :color red)
;;   ("L" shrink-window-horizontally :color red)
;;   ("l" windmove-right :color red)
;;   ("r" winner-redo :color red)
;;   ("s" split-window-vertically :color red)
;;   ("u" winner-undo :color red)
;;   ("v" split-window-horizontally :color red)
;;   ("w" other-window)
;;   ("z" delete-other-windows)
;;   )
;; Windows:2 ends here

;; [[file:config.org::*Text zoom][Text zoom:1]]
(map!
 (:leader
  :desc "Open hydra for zoom controls" "wz"
  #'+hydra/text-zoom/body
  )
 )
;; Text zoom:1 ends here

;; [[file:config.org::*File info][File info:2]]
(use-package! file-info
  :ensure t
  :config
  (setq hydra-posframe-show-params
        `(:poshandler posframe-poshandler-frame-center
          :internal-border-width 2
          :left-fringe 16
          :right-fringe 16
          )
        )
  :init
  (map!
   (:leader
    :desc "Show file info" "fi"
    #'file-info-show
    )
   )
  )
;; File info:2 ends here

;; [[file:config.org::*Math hydra][Math hydra:2]]
(provide 'math-hydras)
(use-package! math-hydras)
;; Math hydra:2 ends here

;; [[file:config.org::*Dired hydra][Dired hydra:1]]
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("<ESC>" nil)
  ("." nil :color blue))

(map! :map dired-mode-map
      :ng "?" #'hydra-dired/body
      )
;; Dired hydra:1 ends here

;; [[file:config.org::*Terminal Here][Terminal Here:2]]
(use-package! terminal-here
  :custom
  (terminal-here-linux-terminal-command 'alacritty)
  (terminal-here-mac-terminal-command 'alacritty)
  :init
  (map!
   (:leader
    :desc "Open folder with terminal" "ft"
    #'terminal-here-launch
    )
   (
    :leader
    :desc "Open folder with terminal" "fT"
    #'terminal-here-project-launch)
   )
  )
;; Terminal Here:2 ends here

;; [[file:config.org::*Spell-fu][Spell-fu:1]]
(add-hook
 'spell-fu-mode-hook
 (lambda ()
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "uk"))
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "ru"))
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en-computers"))
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en-science"))
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en-science"))
   ))
;; Spell-fu:1 ends here

;; [[file:config.org::*Just makefiles][Just makefiles:2]]
(use-package! just-mode)
(use-package! justl
  :init
  (setq justl-executable (executable-find "just"))
  )
;; Just makefiles:2 ends here

;; [[file:config.org::*What-the-commit][What-the-commit:2]]
(use-package! what-the-commit)
;; What-the-commit:2 ends here

;; [[file:config.org::*GC-buffers][GC-buffers:2]]
(use-package! gc-buffers)
;; GC-buffers:2 ends here

;; [[file:config.org::*Copilot][Copilot:2]]
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :init
  (map!
   (:leader
    (:prefix-map ("t a" . "Ai related")
     :desc "Toggle copilot" "c" #'copilot-mode
     )
    )
   )
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )
(use-package! copilot-chat)
;; Copilot:2 ends here

;; [[file:config.org::*Lsp tailwind][Lsp tailwind:2]]
;; https://github.com/merrickluo/lsp-tailwindcss/issues/66
;; (use-package! lsp-tailwindcss
;;   ;; :when (modulep! +lsp)
;;   :init
;;   (setq! lsp-tailwindcss-add-on-mode t)
;;   )
;; (use-package! lsp-tailwindcss)
;; Lsp tailwind:2 ends here

;; [[file:config.org::*nginx][nginx:2]]
(use-package nginx-mode
 :commands nginx-mode
 )
(add-hook! 'nginx-mode-hook #'lsp)
;; nginx:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(setq treemacs-width 40)
(setq! treemacs--width-is-locked nil)
(setq! treemacs-width-is-initially-locked nil)
;; Treemacs:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; Treemacs:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:3]]
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
;; Treemacs:3 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:2]]
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))
;; Prettier page breaks:2 ends here

;; [[file:config.org::*Emacs lsp booster][Emacs lsp booster:2]]
(setq lsp-use-plists t)
;; Emacs lsp booster:2 ends here

;; [[file:config.org::*Typst][Typst:2]]
(use-package! typst-ts-mode
  )

(after! typst-ts-mode
  (add-to-list 'treesit-language-source-alist
               '(typst "https://github.com/uben0/tree-sitter-typst"))
  (treesit-install-language-grammar 'typst)
  (set-formatter! 'typstyle '("typstyle" "-i") :modes '(typst-ts-mode))
  )

(setq lsp-typst-enable-backtrace "1")

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '("\\.typ$" . "typst"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "typst-lsp")
                    :activation-fn (lsp-activate-on "typst")
                    :environment-fn (lambda ()
                                      '(("RUST_BACKTRACE" . lsp-typst-enable-backtrace)))
                    :server-id 'typst-lsp)))



(use-package! typst-preview
  )
;; Typst:2 ends here

;; [[file:config.org::*Obsidian][Obsidian:2]]
(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/Knowledge base")
  (global-obsidian-mode t)
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-link'
              ("C-c C-l" . obsidian-insert-wikilink)))
;; Obsidian:2 ends here

;; [[file:config.org::*Haskell][Haskell:1]]
(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "fourmolu"))
;; Haskell:1 ends here

;; [[file:config.org::*Rust][Rust:1]]
(setq-hook! 'rustic-mode-hook +format-with-lsp nil)
;; Rust:1 ends here

;; [[file:config.org::*Js][Js:1]]
(setq-hook! 'js2-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
;; Js:1 ends here

;; [[file:config.org::*Js][Js:2]]
(after! (:and lsp-mode flycheck-mode)
  (flycheck-add-next-checker 'lsp 'javascript-eslint))
;; Js:2 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
;; LaTeX:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:2]]
(setq +latex-viewers '(zathura pdf-tools evince okular skim sumatrapdf))
;; LaTeX:2 ends here

;; [[file:config.org::*Markdown][Markdown:1]]
(add-hook! (gfm-mode markdown-mode) #'mixed-pitch-mode)
;; Markdown:1 ends here

;; [[file:config.org::*Markdown][Markdown:2]]
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
;; Markdown:2 ends here

;; [[file:config.org::*Markdown][Markdown:3]]
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))
;; Markdown:3 ends here

;; [[file:config.org::*Nix][Nix:1]]
(setq-hook! 'nix-mode-hook company-idle-delay nil)
;; Nix:1 ends here

;; [[file:config.org::*Nix][Nix:2]]
;; (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode)) ;; doesn't work for some reason

;; (after! apheleia

;;   (push '(alejandra . ("alejandra" "--quiet" "-"))
;;         apheleia-formatters)
;;   (setf (alist-get 'nix-mode apheleia-mode-alist)
;;         '(alejandra))

;;   )

(setq-hook! 'nix-mode-hook +format-with-lsp nil)
(setq-hook! nix-mode-hook +format-with 'nixfmt)
;; (setq-hook! 'nix-mode-hook +format-with nil)
;; Nix:2 ends here

;; [[file:config.org::*Nix][Nix:3]]
(setq
 lsp-nix-nil-auto-eval-inputs nil
 lsp-nix-nil-max-mem 2560
 )
;; Nix:3 ends here

(after! org
  (setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/roam")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      )
(add-hook! org-mode :append
           ;; #'org-fragtog-mode
           #'variable-pitch-mode
           )
(setq
 org-use-property-inheritance t              ; it's convenient to have properties inherited
 org-log-done 'time                          ; having the time a item is done sounds convininet
 org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
 org-export-in-background t                  ; run export processes in external emacs process
 org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
 org-export-with-sub-superscripts '{}       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
 )
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))
(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-forward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)
(add-hook 'org-mode-hook
          (λ! (yas-minor-mode)
              (yas-activate-extra-mode 'latex-mode)))
(map! :map org-mode-map
      :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))
(defun +org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200B" "" text)))

(add-to-list 'org-export-filter-plain-text-functions #'+org-export-remove-zero-width-space t)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(defadvice! org-edit-latex-emv-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))
(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")
(use-package! oc-csl-activate
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t
        org-cite-csl-activate-use-document-locale t
    )
  (defun +org-cite-csl-activate/enable ()
    (interactive)
    (setq org-cite-activate-processor 'csl-activate)
    (add-hook! 'org-mode-hook '((lambda () (cursor-sensor-mode 1)) org-cite-csl-activate-render-all))
    (defadvice! +org-cite-csl-activate-render-all-silent (orig-fn)
      :around #'org-cite-csl-activate-render-all
      (with-silent-modifications (funcall orig-fn)))
    (when (eq major-mode 'org-mode)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (org-cite-activate (point-max)))
        (org-cite-csl-activate-render-all)))
    (fmakunbound #'+org-cite-csl-activate/enable)))
(after! citar
  (setq org-cite-global-bibliography
        (let ((libfile-search-names '("library.json" "Library.json" "library.bib" "Library.bib"))
              (libfile-dir "~/Zotero")
              paths)
          (dolist (libfile libfile-search-names)
            (when (and (not paths)
                       (file-exists-p (expand-file-name libfile libfile-dir)))
              (setq paths (list (expand-file-name libfile libfile-dir)))))
          paths)
        citar-bibliography org-cite-global-bibliography
        citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
(after! oc-csl
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))
(after! oc
  (setq org-cite-export-processors '((t csl))))
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert citation" "@" #'org-cite-insert)
(after! oc
  (defun org-ref-to-org-cite ()
    "Attempt to convert org-ref citations to org-cite syntax."
    (interactive)
    (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                               ("nocite" . "/n")
                               ("citep" . "") ("citep*" . "//f")
                               ("parencite" . "") ("Parencite" . "//c")
                               ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                               ("citeyear" . "/na/b")
                               ("Citep" . "//c") ("Citealp" . "//bc")
                               ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                               ("autocite" . "") ("Autocite" . "//c")
                               ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                               ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
           (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                            ":" (group (+ (not (any "\n 	,.)]}")))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cite-regexp nil t)
          (message (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2)))
          (replace-match (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2))))))))
(setq org-cite-csl-locales-dir "~/code/cloned/csl-locales")
(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Work"
                           :tag "work"
                           :priority "A"
                           :order 3)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "To watch"
                           :tag "Watch"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
(defun +yas/org-src-header-p ()
  "Determine whether `point' is within a src-block header or header-args."
  (pcase (org-element-type (org-element-context))
    ('src-block (< (point) ; before code part of the src-block
                   (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                   (forward-line 1)
                                   (point))))
    ('inline-src-block (< (point) ; before code part of the inline-src-block
                          (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                          (search-forward "]{")
                                          (point))))
    ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))
(defun +yas/org-prompt-header-arg (arg question values)
  "Prompt the user to set ARG header property to one of VALUES with QUESTION.
The default value is identified and indicated. If either default is selected,
or no selection is made: nil is returned."
  (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
         (default
           (or
            (cdr (assoc arg
                        (if src-block-p
                            (nth 2 (org-babel-get-src-block-info t))
                          (org-babel-merge-params
                           org-babel-default-header-args
                           (let ((lang-headers
                                  (intern (concat "org-babel-default-header-args:"
                                                  (+yas/org-src-lang)))))
                             (when (boundp lang-headers) (eval lang-headers t)))))))
            ""))
         default-value)
    (setq values (mapcar
                  (lambda (value)
                    (if (string-match-p (regexp-quote value) default)
                        (setq default-value
                              (concat value " "
                                      (propertize "(default)" 'face 'font-lock-doc-face)))
                      value))
                  values))
    (let ((selection (consult--read values :prompt question :default default-value)))
      (unless (or (string-match-p "(default)$" selection)
                  (string= "" selection))
        selection))))
(defun +yas/org-src-lang ()
  "Try to find the current language of the src/header at `point'.
Return nil otherwise."
  (let ((context (org-element-context)))
    (pcase (org-element-type context)
      ('src-block (org-element-property :language context))
      ('inline-src-block (org-element-property :language context))
      ('keyword (when (string-match "^header-args:\\([^ ]+\\)" (org-element-property :value context))
                  (match-string 1 (org-element-property :value context)))))))

(defun +yas/org-last-src-lang ()
  "Return the language of the last src-block, if it exists."
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
      (org-element-property :language (org-element-context)))))

(defun +yas/org-most-common-no-property-lang ()
  "Find the lang with the most source blocks that has no global header-args, else nil."
  (let (src-langs header-langs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
        (push (+yas/org-src-lang) src-langs))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+property: +header-args" nil t)
        (push (+yas/org-src-lang) header-langs)))

    (setq src-langs
          (mapcar #'car
                  ;; sort alist by frequency (desc.)
                  (sort
                   ;; generate alist with form (value . frequency)
                   (cl-loop for (n . m) in (seq-group-by #'identity src-langs)
                            collect (cons n (length m)))
                   (lambda (a b) (> (cdr a) (cdr b))))))

    (car (cl-set-difference src-langs header-langs :test #'string=))))
(defun org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))
(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  (ignore-errors (apply orig-fn args)))
(use-package! ob-mermaid
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)))
  ;; (setq ob-mermaid-cli-path (executable-find "mmdc"))
  )
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))
(use-package! org-media-note
  :init (setq org-media-note-use-org-ref t)
  :hook (org-mode .  org-media-note-mode)
  :bind (
         ("H-v" . org-media-note-hydra/body))
  :config
  (setq org-media-note-screenshot-image-dir org-attach-id-dir)  ;; Folder to save screenshot
  (setq org-media-note-use-refcite-first t)
  )
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
   org-modern-table-vertical 1
   org-modern-table-horizontal 0.2
   org-modern-todo-faces
   '(("TODO" :inverse-video t :inherit org-todo)
     ("PROJ" :inverse-video t :inherit +org-todo-project)
     ("STRT" :inverse-video t :inherit +org-todo-active)
     ("[-]"  :inverse-video t :inherit +org-todo-active)
     ("HOLD" :inverse-video t :inherit +org-todo-onhold)
     ("WAIT" :inverse-video t :inherit +org-todo-onhold)
     ("[?]"  :inverse-video t :inherit +org-todo-onhold)
     ("KILL" :inverse-video t :inherit +org-todo-cancel)
     ("NO"   :inverse-video t :inherit +org-todo-cancel))
   org-modern-footnote
   (cons nil (cadr org-script-display))
   org-modern-block-fringe t
   org-modern-block-name
   '((t . t)
     ("src" "»" "«")
     ("example" "»–" "–«")
     ("quote" "❝" "❞")
     ("export" "⏩" "⏪"))
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule (make-string 36 ?─)
   org-modern-keyword
   '((t . t)
     ("title" . "𝙏")
     ("subtitle" . "𝙩")
     ("author" . "𝘼")
     ("email" . #("" 0 1 (display (raise -0.14))))
     ("date" . "𝘿")
     ("property" . "☸")
     ("options" . "⌥")
     ("startup" . "⏻")
     ("macro" . "𝓜")
     ("bind" . #("" 0 1 (display (raise -0.1))))
     ("bibliography" . "")
     ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
     ("cite_export" . "⮭")
     ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
     ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
     ("include" . "⇤")
     ("setupfile" . "⇚")
     ("html_head" . "🅷")
     ("html" . "🅗")
     ("latex_class" . "🄻")
     ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
     ("latex_header" . "🅻")
     ("latex_header_extra" . "🅻⁺")
     ("latex" . "🅛")
     ("beamer_theme" . "🄱")
     ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
     ("beamer_font_theme" . "🄱𝐀")
     ("beamer_header" . "🅱")
     ("beamer" . "🅑")
     ("attr_latex" . "🄛")
     ("attr_html" . "🄗")
     ("attr_org" . "⒪")
     ("call" . #("" 0 1 (display (raise -0.15))))
     ("name" . "⁍")
     ("header" . "›")
     ("caption" . "☰")
     ("RESULTS" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))
(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))
(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
(custom-set-faces!
  '(org-document-title :height 1.2))
(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))
(setq org-fontify-quote-and-verse-blocks t)
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)
(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                             (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))
(defvar org-prettify-inline-results t
  "Whether to use (ab)use prettify-symbols-mode on {{{results(...)}}}.
Either t or a cons cell of strings which are used as substitutions
for the start and end of inline results, respectively.")

(defvar org-fontify-inline-src-blocks-max-length 200
  "Maximum content length of an inline src block that will be fontified.")

(defun org-fontify-inline-src-blocks (limit)
  "Try to apply `org-fontify-inline-src-blocks-1'."
  (condition-case nil
      (org-fontify-inline-src-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
                    (current-buffer)
                    (line-number-at-pos)))))

(defun org-fontify-inline-src-blocks-1 (limit)
  "Fontify inline src_LANG blocks, from `point' up to LIMIT."
  (let ((case-fold-search t)
        (initial-point (point)))
    (while (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t) ; stolen from `org-element-inline-src-block-parser'
      (let ((beg (match-beginning 0))
            pt
            (lang-beg (match-beginning 1))
            (lang-end (match-end 1)))
        (remove-text-properties beg lang-end '(face nil))
        (font-lock-append-text-property lang-beg lang-end 'face 'org-meta-line)
        (font-lock-append-text-property beg lang-beg 'face 'shadow)
        (font-lock-append-text-property beg lang-end 'face 'org-block)
        (setq pt (goto-char lang-end))
        ;; `org-element--parse-paired-brackets' doesn't take a limit, so to
        ;; prevent it searching the entire rest of the buffer we temporarily
        ;; narrow the active region.
        (save-restriction
          (narrow-to-region beg (min (point-max) limit (+ lang-end org-fontify-inline-src-blocks-max-length)))
          (when (ignore-errors (org-element--parse-paired-brackets ?\[))
            (remove-text-properties pt (point) '(face nil))
            (font-lock-append-text-property pt (point) 'face 'org-block)
            (setq pt (point)))
          (when (ignore-errors (org-element--parse-paired-brackets ?\{))
            (remove-text-properties pt (point) '(face nil))
            (font-lock-append-text-property pt (1+ pt) 'face '(org-block shadow))
            (unless (= (1+ pt) (1- (point)))
              (if org-src-fontify-natively
                  (org-src-font-lock-fontify-block (buffer-substring-no-properties lang-beg lang-end) (1+ pt) (1- (point)))
                (font-lock-append-text-property (1+ pt) (1- (point)) 'face 'org-block)))
            (font-lock-append-text-property (1- (point)) (point) 'face '(org-block shadow))
            (setq pt (point))))
        (when (and org-prettify-inline-results (re-search-forward "\\= {{{results(" limit t))
          (font-lock-append-text-property pt (1+ pt) 'face 'org-block)
          (goto-char pt))))
    (when org-prettify-inline-results
      (goto-char initial-point)
      (org-fontify-inline-src-results limit))))

(defun org-fontify-inline-src-results (limit)
  (while (re-search-forward "{{{results(\\(.+?\\))}}}" limit t)
    (remove-list-of-text-properties (match-beginning 0) (point)
                                    '(composition
                                      prettify-symbols-start
                                      prettify-symbols-end))
    (font-lock-append-text-property (match-beginning 0) (match-end 0) 'face 'org-block)
    (let ((start (match-beginning 0)) (end (match-beginning 1)))
      (with-silent-modifications
        (compose-region start end (if (eq org-prettify-inline-results t) "⟨" (car org-prettify-inline-results)))
        (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))
    (let ((start (match-end 1)) (end (point)))
      (with-silent-modifications
        (compose-region start end (if (eq org-prettify-inline-results t) "⟩" (cdr org-prettify-inline-results)))
        (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))))

(defun org-fontify-inline-src-blocks-enable ()
  "Add inline src fontification to font-lock in Org.
Must be run as part of `org-font-lock-set-keywords-hook'."
  (setq org-font-lock-extra-keywords
        (append org-font-lock-extra-keywords '((org-fontify-inline-src-blocks)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-fontify-inline-src-blocks-enable)
;; (use-package org-pretty-tags
;; :config
;;  (setq org-pretty-tags-surrogate-strings
;;        `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
;;          ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
;;          ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
;;          ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
;;          ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
;;          ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
;;  (org-pretty-tags-global-mode))

(after! org-superstar
  (setq ;; org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))
(appendq! +ligatures-extra-symbols
          (list
           :list_property "∷"
           :em_dash       "—"
           :ellipses      "…"
           :arrow_right   "→"
           :arrow_left    "←"
           ;; :title         "𝙏"
           ;; :subtitle      "𝙩"
           ;; :author        "𝘼"
           ;; :date          "𝘿"
           ;; :latex_class   "🄲"
           ;; :attr_latex    "🄛"
           ;; :attr_html     "🄗"
           ;; :begin_quote   "❝"
           ;; :end_quote     "❞"
           ;; :caption       "☰"
           ;; :header        "›"
           ;; :bibliography  ""
           ;; :print_biblio  ""
           )
          )

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))

;; (set-ligatures! 'org-mode
;;   :merge t
;;   :list_property "::"
;;   :em_dash       "---"
;;   :ellipsis      "..."
;;   :arrow_right   "->"
;;   :arrow_left    "<-"
;;   ;; :title         "#+title:"
;;   ;; :subtitle      "#+subtitle:"
;;   ;; :author        "#+author:"
;;   ;; :date          "#+date:"
;;   ;; :latex_class   "#+latex_class:"
;;   ;; :attr_latex    "#+attr_latex:"
;;   ;; :attr_html     "#+attr_html:"
;;   ;; :begin_quote   "#+begin_quote"
;;   ;; :end_quote     "#+end_quote"
;;   ;; :caption       "#+caption:"
;;   ;; :header        "#+header:"
;;   ;; :bibliography  "#+bibliography:"
;;   ;; :print-biblio  "#+print_bibliography:"
;;   )
;; (plist-put +ligatures-extra-symbols :name "⁍")
(setq org-highlight-latex-and-related '(native script entities))
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
;; (use-package! org-fragtog
;;   :hook (org-mode . org-fragtog-mode))

;; (add-hook 'org-mode-hook #'org-latex-preview-auto-mode)
;; (add-hook 'latex-mode-hook #'xenops-mode)
;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
;; (add-hook 'org-mode-hook   #'xenops-mode)
(plist-put org-format-latex-options :background "Transparent")
;; Calibrated based on the TeX font and org-buffer font.
(plist-put org-format-latex-options :zoom 0.93)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvipng :programs
         ("lualatex" "dvipng")
         :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
         (1.0 . 1.0)
         :latex-compiler
         ("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
        ;; (dvisvgm :programs
        ;;          ("tectonic" "dvisvgm")
        ;;          :description "dvi > svg" :message "you need to install the programs: tectonic and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
        ;;          (1.7 . 1.5)
        ;;          :latex-compiler
        ;;          "tectonic -X compile %f -Z shell-escape -Z continue-on-errors --outfmt xdv --outdir %o "
        ;;          :image-converter
        ;;          ("dvisvgm %f -n -b min -c %S -o %O"))
        (dvisvgm :programs
           ("latex" "dvisvgm")
           :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
           (1.7 . 1.5)
           :latex-compiler
           ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs
                     ("latex" "convert")
                     :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                     (1.0 . 1.0)
                     :latex-compiler
                     ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter
                     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

;; xenops
;; (setq
;;  xenops-math-latex-process org-preview-latex-default-process
;;  xenops-math-latex-process-alist org-preview-latex-process-alist
;;  xenops-math-image-scale-factor 2
;;  )
;; (setq org-src-block-faces
;;         '(("latex" (:background "unspecified"))))
(defun +org-mode--fontlock-only-mode ()
  "Just apply org-mode's font-lock once."
  (let (org-mode-hook
        org-hide-leading-stars
        org-hide-emphasis-markers)
    (org-set-font-lock-defaults)
    (font-lock-ensure))
  (setq-local major-mode #'fundamental-mode))

(defun +org-export-babel-mask-org-config (_backend)
  "Use `+org-mode--fontlock-only-mode' instead of `org-mode'."
  (setq-local org-src-lang-modes
              (append org-src-lang-modes
                      (list (cons "org" #'+org-mode--fontlock-only)))))

(add-hook 'org-export-before-processing-hook #'+org-export-babel-mask-org-config)
(setq org-export-headline-levels 5) ; I like nesting
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
(after! ox-latex
  (add-to-list 'org-latex-classes
               '("scr-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("blank"
                 "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("bmc-article"
                 "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("bmc"
                 "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("memoir" "\\documentclass{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("scrlttr2" "\\documentclass{scrlttr2}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(setq org-latex-default-class "scr-article"
      org-latex-tables-booktabs t
      )
(defvar org-latex-extra-special-string-regexps
  '(("->" . "\\\\textrightarrow{}")
    ("<-" . "\\\\textleftarrow{}")))

(defun org-latex-convert-extra-special-strings (string)
  "Convert special characters in STRING to LaTeX."
  (dolist (a org-latex-extra-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defadvice! org-latex-plain-text-extra-special-a (orig-fn text info)
  "Make `org-latex-plain-text' handle some extra special strings."
  :around #'org-latex-plain-text
  (let ((output (funcall orig-fn text info)))
    (when (plist-get info :with-special-strings)
      (setq output (org-latex-convert-extra-special-strings output)))
    output))
(setq org-latex-text-markup-alist
      '((bold . "\\textbf{%s}")
        (code . protectedtexttt)
        (italic . "\\emph{%s}")
        (strike-through . "\\sout{%s}")
        (underline . "\\uline{%s}")
        (verbatim . verb)))
(defadvice! +org-latex-link (orig-fn link desc info)
  "Acts as `org-latex-link', but supports remote images."
  :around #'org-latex-link
  (setq o-link link
        o-desc desc
        o-info info)
  (if (and (member (plist-get (cadr link) :type) '("http" "https"))
           (member (file-name-extension (plist-get (cadr link) :path))
                   '("png" "jpg" "jpeg" "pdf" "svg")))
      (org-latex-link--remote link desc info)
    (funcall orig-fn link desc info)))

(defun org-latex-link--remote (link _desc info)
  (let* ((url (plist-get (cadr link) :raw-link))
         (ext (file-name-extension url))
         (target (format "%s%s.%s"
                         (temporary-file-directory)
                         (replace-regexp-in-string "[./]" "-"
                                                   (file-name-sans-extension (substring (plist-get (cadr link) :path) 2)))
                         ext)))
    (unless (file-exists-p target)
      (url-copy-file url target))
    (setcdr link (--> (cadr link)
                   (plist-put it :type "file")
                   (plist-put it :path target)
                   (plist-put it :raw-link (concat "file:" target))
                   (list it)))
    (concat "% fetched from " url "\n"
            (org-latex--inline-image link info))))
(setq org-latex-listings 'engraved) ; NOTE non-standard value
(defvar-local org-export-has-code-p nil)

(defadvice! org-export-expect-no-code (&rest _)
  :before #'org-export-as
  (setq org-export-has-code-p nil))

(defadvice! org-export-register-code (&rest _)
  :after #'org-latex-src-block
  :after #'org-latex-inline-src-block-engraved
  (setq org-export-has-code-p t))
(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))
;; (setq org-latex-pdf-process
;;       '("tectonic %f"))
(setq org-beamer-theme "[progressbar=foot]metropolis")
(setq org-beamer-frame-level 2)
(setq org-ascii-charset 'utf-8)
(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))
(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally."))

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
   org-modern-table-vertical 1
   org-modern-table-horizontal 0.2
   org-modern-todo-faces
   '(("TODO" :inverse-video t :inherit org-todo)
     ("PROJ" :inverse-video t :inherit +org-todo-project)
     ("STRT" :inverse-video t :inherit +org-todo-active)
     ("[-]"  :inverse-video t :inherit +org-todo-active)
     ("HOLD" :inverse-video t :inherit +org-todo-onhold)
     ("WAIT" :inverse-video t :inherit +org-todo-onhold)
     ("[?]"  :inverse-video t :inherit +org-todo-onhold)
     ("KILL" :inverse-video t :inherit +org-todo-cancel)
     ("NO"   :inverse-video t :inherit +org-todo-cancel))
   org-modern-footnote
   (cons nil (cadr org-script-display))
   org-modern-block-fringe t
   org-modern-block-name
   '((t . t)
     ("src" "»" "«")
     ("example" "»–" "–«")
     ("quote" "❝" "❞")
     ("export" "⏩" "⏪"))
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule (make-string 36 ?─)
   org-modern-keyword
   '((t . t)
     ("title" . "𝙏")
     ("subtitle" . "𝙩")
     ("author" . "𝘼")
     ("email" . #("" 0 1 (display (raise -0.14))))
     ("date" . "𝘿")
     ("property" . "☸")
     ("options" . "⌥")
     ("startup" . "⏻")
     ("macro" . "𝓜")
     ("bind" . #("" 0 1 (display (raise -0.1))))
     ("bibliography" . "")
     ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
     ("cite_export" . "⮭")
     ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
     ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
     ("include" . "⇤")
     ("setupfile" . "⇚")
     ("html_head" . "🅷")
     ("html" . "🅗")
     ("latex_class" . "🄻")
     ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
     ("latex_header" . "🅻")
     ("latex_header_extra" . "🅻⁺")
     ("latex" . "🅛")
     ("beamer_theme" . "🄱")
     ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
     ("beamer_font_theme" . "🄱𝐀")
     ("beamer_header" . "🅱")
     ("beamer" . "🅑")
     ("attr_latex" . "🄛")
     ("attr_html" . "🄗")
     ("attr_org" . "⒪")
     ("call" . #("" 0 1 (display (raise -0.15))))
     ("name" . "⁍")
     ("header" . "›")
     ("caption" . "☰")
     ("RESULTS" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! engrave-faces-latex
  :config
  (setq engrave-faces-preset-styles (engrave-faces-generate-preset))
  :after ox-latex)

(after! ox-ascii
  (defvar org-ascii-convert-latex t
    "Use latex2text to convert LaTeX elements to unicode.")

  (defadvice! org-ascii-latex-environment-unicode-a (latex-environment _contents info)
    "Transcode a LATEX-ENVIRONMENT element from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
    :override #'org-ascii-latex-environment
    (when (plist-get info :with-latex)
      (org-ascii--justify-element
       (org-remove-indentation
        (let* ((latex (org-element-property :value latex-environment))
               (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                             org-ascii-convert-latex
                             (doom-call-process "latex2text" "-q" "--code" latex))))
          (if (= (car unicode) 0) ; utf-8 set, and sucessfully ran latex2text
              (cdr unicode) latex)))
       latex-environment info)))

  (defadvice! org-ascii-latex-fragment-unicode-a (latex-fragment _contents info)
    "Transcode a LATEX-FRAGMENT object from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
    :override #'org-ascii-latex-fragment
    (when (plist-get info :with-latex)
      (let* ((latex (org-element-property :value latex-fragment))
             (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                           org-ascii-convert-latex
                             (doom-call-process "latex2text" "-q" "--code" latex))))
        (if (and unicode (= (car unicode) 0)) ; utf-8 set, and sucessfully ran latex2text
            (cdr unicode) latex)))))

;; [[file:config.org::*Ansi colours][Ansi colours:1]]
(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))
;; Ansi colours:1 ends here

;; [[file:config.org::*Ansi colours][Ansi colours:2]]
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))
;; Ansi colours:2 ends here

;; [[file:config.org::*Python][Python:1]]
(after! lsp-python-ms
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (set-lsp-priority! 'mspyls 1))
;; Python:1 ends here

;; [[file:config.org::*Rust][Rust:1]]
(setq rustic-lsp-server 'rust-analyzer)
;; Rust:1 ends here

;; [[file:config.org::*CSharp][CSharp:1]]
(use-package! html-mode
  :mode "\\.cshtml\\'"
  )
;; CSharp:1 ends here

;; [[file:config.org::*HTML][HTML:1]]
(setq-hook! 'web-mode-hook +format-with-lsp nil)
(setq-hook! 'web-mode-hook +format-with 'prettier)
;; HTML:1 ends here

;; [[file:config.org::*Org][Org:1]]
(defun leniviy/org-save-and-export-pdf ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-latex-export-to-pdf :async t)))
;; Org:1 ends here

;; [[file:config.org::*Org][Org:2]]
(defun leniviy/org-save-and-export-latex ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-latex-export-to-latex :async t)))
(defun leniviy/org-save-and-export-beamer ()
  (if (eq major-mode 'org-mode)
      (org-beamer-export-to-latex :async t)))
;; Org:2 ends here

;; [[file:config.org::*Org][Org:3]]
(defun config-tangle ()
  (interactive)
  (shell-command
   "emacs --batch --eval \"(require 'org)\" --eval '(org-babel-tangle-file \"~/.config/doom/config.org\")'"
   ;; (concat "org-tangle " doom-user-dir "config.org" " &")
   )
  )

()
;; Org:3 ends here

;; [[file:config.org::*Fixes][Fixes:1]]
(setq flymake-allowed-file-name-masks '())
;; Fixes:1 ends here
