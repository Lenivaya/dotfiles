;; -*- no-byte-compile: t; -*-

(package! selectric-mode)

(package! speed-type)

(package! spray)

(package! keycast)

(package! gif-screencast)

(package! calctex
  :recipe (:host github :repo "johnbcoughlin/calctex"
           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el"))
  )

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot" :build (:not compile)))

(package! reverse-im)

(package! kdeconnect)

(package! imenu-list)

(package! deadgrep)

(package! explain-pause-mode)

(package! go-translate)

(package! timesheet)

(package! wakatime-mode)

(package! iscroll :recipe (:host github
                           :repo "casouri/iscroll"))

(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))

(package! org-super-agenda)

(package! doct
  :recipe (:host github :repo "progfolio/doct")
  )

(package! org-fragtog)

(package! xenops)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-pretty-tags)

(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! ox-gfm)

(package! org-ref)

(package! org-chef)

(package! org-pandoc-import :recipe
  (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

(package! ob-mermaid)

(package! org-modern)

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac")

(package! org-roam-ui)
(package! websocket)

(package! org-transclusion)

(package! pretty-hydra)  ;; dependency
(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note"))

(package! graphviz-dot-mode :pin "8ff793b13707cb511875f56e167ff7f980a31136")

(package! authinfo-color-mode
  :recipe (:host github :repo  "tecosaur/authinfo-color-mode"))

;; [[file:config.org::*EVIL][EVIL:2]]
(package! evil-escape :disable t)
;; EVIL:2 ends here

;; [[file:config.org::*Systemd][Systemd:1]]
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
;; Systemd:1 ends here

;; [[file:config.org::*Leetcode][Leetcode:1]]
;; (package! leetcode)
;; Leetcode:1 ends here

;; [[file:config.org::*Fountain][Fountain:1]]
(package! fountain-mode)
;; Fountain:1 ends here

;; [[file:config.org::*Open-With][Open-With:1]]
(package! crux)
;; Open-With:1 ends here

;; [[file:config.org::*Spray][Spray:1]]
(package! spray :pin "74d9dcfa2e8b38f96a43de9ab0eb13364300cb46")
;; Spray:1 ends here

;; [[file:config.org::*Git blame][Git blame:1]]
(package! blamer :recipe (:host github :repo "artawower/blamer.el"))
;; Git blame:1 ends here

;; [[file:config.org::*Hydra Posframe][Hydra Posframe:1]]
(package! hydra-posframe :recipe (:host github :repo "Ladicle/hydra-posframe"))
;; Hydra Posframe:1 ends here

;; [[file:config.org::*File info][File info:1]]
(package! file-info)
;; File info:1 ends here

;; [[file:config.org::*Math hydra][Math hydra:1]]
(package! math-hydras
  :recipe (
           :host github
           :repo "ashok-khanna/math-hydras"
           :files ("math-hydras.el")
           )
  )
;; Math hydra:1 ends here

;; [[file:config.org::*Terminal Here][Terminal Here:1]]
(package! terminal-here)
;; Terminal Here:1 ends here

;; [[file:config.org::*Just makefiles][Just makefiles:1]]
(package! just-mode)
(package! justl)
;; Just makefiles:1 ends here

;; [[file:config.org::*What-the-commit][What-the-commit:1]]
(package! what-the-commit)
;; What-the-commit:1 ends here

;; [[file:config.org::*GC-buffers][GC-buffers:1]]
(package! gc-buffers :recipe (:type git :repo "https://codeberg.org/akib/emacs-gc-buffers.git"))
;; GC-buffers:1 ends here

;; [[file:config.org::*Copilot][Copilot:1]]
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
;; Copilot:1 ends here

;; [[file:config.org::*Lsp tailwind][Lsp tailwind:1]]
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
;; Lsp tailwind:1 ends here

;; [[file:config.org::*nginx][nginx:1]]
(package! nginx-mode)
;; nginx:1 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines")
  :pin "79eca86e0634ac68af862e15c8a236c37f446dcd")
;; Prettier page breaks:1 ends here

;; [[file:config.org::*Typst][Typst:1]]
(package! typst-ts-mode :recipe (:host sourcehut :repo "meow_king/typst-ts-mode"))
(package! typst-preview
  :recipe (:host github :repo "havarddj/typst-preview.el"))
;; Typst:1 ends here

;; [[file:config.org::*Obsidian][Obsidian:1]]
(package! obsidian)
;; Obsidian:1 ends here

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate") :pin "9e68d9204469c674f49a20bdf7ea85da4f4bf720")

(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))
