;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       ;; (company                     ; the ultimate code completion backend
       ;;  +childframe)                ; ... when your children are better than you
       ;;helm                       ; the *other* search engine for love and life
       ;;ido                        ; the other *other* search engine...
       ;; (ivy                         ; a search engine for love and life
       ;;  +icons                      ; ... icons are nice
       ;;  +childframe                 ; ... when your children are better than you
       ;;  +prescient)                 ; ... I know what I want(ed)
       (corfu
        +icons
        +orderless
        +dabbrev
        )
       (vertico +icons
                ;; +childframe
                )

       :ui
       ;;deft                       ; notational velocity for Emacs
       doom                         ; what makes DOOM look the way it does
       doom-dashboard               ; a nifty splash screen for Emacs
       doom-quit                    ; DOOM quit-message prompts when you quit Emacs
       (emoji                       ; 🙂
        +unicode
        +github)
       ;;fill-column                ; a `fill-column' indicator
       hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra                      ; quick documentation for related commands
       indent-guides              ; highlighted indent columns, notoriously slow
       (ligatures
        +pragmata-pro
        +iosevka
        +extra
        )           ; ligatures and symbols to make your code pretty again
       ;; minimap                    ; show a map of the code on the side
       (modeline
        ;; +light
        )                     ; snazzy, Atom-inspired modeline, plus API
       nav-flash                    ; blink the current line after jumping
       ;;neotree                    ; a project drawer, like NERDTree for vim
       ophints                      ; highlight the region an operation acts on
       (popup                       ; tame sudden yet inevitable temporary windows
        +all                        ; catch all popups that start with an asterix
        +defaults)                  ; default popup rules
       ;;(tabs                      ; an tab bar for Emacs
       ;;  +centaur-tabs)           ; ... with prettier tabs
       (treemacs                     ; a project drawer, like neotree but cooler
        +lsp)
       unicode                    ; extended unicode support for various languages
       (vc-gutter                    ; vcs diff in the fringe
        +pretty
        +diff-hl
        )
       vi-tilde-fringe              ; fringe tildes to mark beyond EOB
       window-select                ; visually switch windows
       workspaces                   ; tab emulation, persistence & separate workspaces
       zen                          ; distraction-free coding or writing

       :editor
       (evil +everywhere)           ; come to the dark side, we have cookies
       file-templates               ; auto-snippets for empty files
       fold                         ; (nigh) universal code folding
       format             ; automated prettiness
       ;;god                        ; run Emacs commands without modifier keys
       ;;lispy                      ; vim for lisp, for people who don't like vim
       multiple-cursors             ; editing in many places at once
       ;;objed                      ; text object editing for the innocent
       ;;parinfer                   ; turn lisp into python, sort of
       rotate-text                  ; cycle region at point between text candidates
       snippets                     ; my elves. They type so I don't have to
       ;;word-wrap                  ; soft wrapping with language-aware indent

       :emacs
       (dired
        +icons
        ;; +ranger
        )               ; making dired pretty [functional]
       electric                     ; smarter, keyword-based electric-indent
       (ibuffer +icons)             ; interactive buffer management
       (undo +tree)                 ; persistent, smarter undo for your inevitable mistakes
       vc                           ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;; shell             ; a terminal REPL for Emacs
       ;; term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       (syntax                       ; tasing you for every semicolon you forget
        +flymake
        +childframe
        +icons
        )
       (spell
        ;; +enchant
        )                        ; tasing you for misspelling mispelling
       grammar                      ; tasing grammar mistake every you make

       :tools
       ;; ansible                      ; a crucible for infrastructure as code
       biblio
       ;; (
       debugger                     ; FIXME stepping through code, to help you add bugs
       ;;+lsp)
       direnv                     ; be direct about your environment
       (docker +lsp)                     ; port everything to containers
       editorconfig               ; let someone else argue about tabs vs spaces
       ;;ein                        ; tame Jupyter notebooks with emacs
       (eval +overlay)              ; run code, run (also, repls)
       gist                       ; interacting with github gists
       (lookup                      ; helps you navigate your code and documentation
        +dictionary                 ; dictionary/thesaurus is nice
        +docsets)                   ; ...or in Dash docsets locally
       (lsp +peek)                          ; Language Server Protocol
       (magit                       ; a git porcelain for Emacs
        ;; +forge
        )                     ; interface with git forges
       make                         ; run make tasks from Emacs
       pass                       ; password manager for nerds
       pdf                          ; pdf enhancements
       ;;prodigy                    ; FIXME managing external services & code builders
       rgb                          ; creating color strings
       ;;taskrunner                 ; taskrunner for all your projects
       ;;terraform                  ; infrastructure as code
       tmux                       ; an API for interacting with tmux
       upload                       ; map local to remote projects via ssh/ftp
       tree-sitter

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty +osc)                          ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp +tree-sitter)                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       (csharp
        +lsp
        ;; +unity
        +tree-sitter
        +dotnet)            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;; (dart +flutter +lsp)   ; paint ui and not much else
       ;;erlang            ; an elegant language for a more civilized age
       ess                          ; emacs speaks statistics
       (elixir
        +lsp
        )                                        ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;; (fsharp           ; ML stands for Microsoft's Language
       ;;  +lsp
       ;;  )
       (go +lsp +tree-sitter)                ; the hipster dialect
       (haskell             ; a language that's lazier than I am
        +lsp
        +tree-sitter
        )
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript
        +lsp
        +tree-sitter
        )        ; all(hope(abandon(ye(who(enter(here))))))
       
       (json +lsp +tree-sitter)
       ;;julia             ; a better, faster MATLAB
       ;; (kotlin +lsp +tree-sitter)            ; a better, slicker Java(Script)
       (latex
        +lsp
        +cdlatex
        +latexmk
        +fold)            ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       (markdown +grip)         ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix
        +tree-sitter
        +lsp
        )               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        ;; +pretty       ; Disable since I use org-modern
        +pandoc
        +dragndrop       ; file drag & drop support
        +noter                      ; enhanced PDF notetaking
        +present         ; using Emacs for presentations
        +gnuplot
        +journal
        +roam2
        )
       ;;perl              ; write code no one else can comprehend
       ;; (php +lsp +tree-sitter)               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python
        +lsp
        +pyright
        +poetry
        +tree-sitter
        )           ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       (rest +jq)              ; Emacs as a REST client
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust
        +lsp
        +tree-sitter
        )            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; (scala +lsp)             ; java, but good
       scheme            ; a fully conniving family of lisps
       (sh +lsp +tree-sitter)                ; she sells {ba,z,fi}sh shells on the C xor
       solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp +tree-sitter)              ; the tubes
       (yaml +lsp +tree-sitter)
       (graphql
        +lsp)
       ;;vala              ; GObjective-C

       :email
       ;; (mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       everywhere                   ; *leave* Emacs!? You must be joking.
       ;; irc                          ; how neckbeards socialize
       ;; (rss +org)                   ; emacs as an RSS reader
       ;;twitter                    ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens)
       )

(setenv "LSP_USE_PLISTS" "1")

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
