(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((jinx-local-words . "confpkg confpkgs smartparens tempbuffer")))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-safe-remote-resources
   '("\\`https://demenses\\.net\\(?:/\\|\\'\\)" "\\`https://b-coimbra\\.github\\.io\\(?:/\\|\\'\\)" "\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(safe-local-variable-values
   '((org-export-babel-evaluate)
     (eval add-to-list 'org-latex-classes
      '("custom-scr-article" "\\documentclass{scrartcl}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
     (eval add-to-list 'org-latex-classes
      '("custom-scr-article" "\\documentclass{scrartcl}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
 '(screenshot-font-size 10)
 '(screenshot-max-width 240))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:height 1.25 :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.08 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.0 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 0.9 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 0.75 :weight extra-bold :inherit markdown-header-face))))
 '(org-document-title ((t (:height 1.2))))
 '(org-modern-statistics ((t (:inherit org-checkbox-statistics-todo))))
 '(outline-1 ((t (:weight extra-bold :height 1.25))))
 '(outline-2 ((t (:weight bold :height 1.15))))
 '(outline-3 ((t (:weight bold :height 1.12))))
 '(outline-4 ((t (:weight semi-bold :height 1.09))))
 '(outline-5 ((t (:weight semi-bold :height 1.06))))
 '(outline-6 ((t (:weight semi-bold :height 1.03))))
 '(outline-8 ((t (:weight semi-bold))))
 '(outline-9 ((t (:weight semi-bold))))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
