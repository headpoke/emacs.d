;;; init.el --- -*- lexical-binding: t -*-

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq user-full-name         "Knut Berg"
      user-mail-address      "knut.berg@nord.no"
      calendar-latitude      67.289
      calendar-longitude     14.560
      calendar-location-name "Bodø, Norway")

(defvar knube-gc-cons-threshold (* 32 1024 1024)) ; increase if stuttering
                                                  ; occurs, decrease if freezing
                                                  ; occurs
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold       knube-gc-cons-threshold
                  gc-cons-percentage      0.1
	          debug-on-error          nil
                  file-name-handler-alist startup-file-name-handler-alist)
            (makunbound 'startup-file-name-handler-alist)))

(defconst *is-a-mac* (eq system-type 'darwin))

(when (version< emacs-version "27.1")
  (warn "Your Emacs might be too old -- This setup was made for
	 version 27.1 or newer. Proceed with caution!"))
(when (not *is-a-mac*)
  (warn "This config was made for my Macbook Air with a UK
         English keyboard layout. Proceed with caution."))

(setq straight-recipes-gnu-elpa-use-mirror t
      straight-repository-branch           "develop")
;;        straight-vc-git-default-clone-depth  1
;;        straight-check-for-modifications     '(find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-enable-use-package-integration t
      straight-use-package-by-default         t)

(straight-use-package 'org-plus-contrib)
(straight-use-package '(org :local-repo nil))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  )

(setq global-mark-ring-max 500
      mark-ring-max        500
      kill-ring-max        500)

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-function-modifier nil))

;; Make use of osx trash
(use-package osx-trash
  :if *is-a-mac*
  :config
  (osx-trash-setup)
  (setq-default delete-by-moving-to-trash t))

;; open files from finder in active frame
(setq ns-pop-up-frames nil)

(use-package exec-path-from-shell
  :if *is-a-mac*
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG"
				    "LC_ALL"
				    "GPG_AGENT_INFO"
				    "SSH_AUTH_SOCK")))

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding
(set-language-environment    'utf-8)
(setq locale-coding-system   'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq scroll-step                     1
      scroll-conservatively           101
      scroll-preserve-screen-position 'always
      next-screen-context-lines       5
      debugger-stack-frame-as-list    t
      mouse-wheel-follow-mouse        t
      mouse-wheel-scroll-amount       '(1 ((shift) . 1))
      mouse-wheel-progressive-speed   nil
      mouse-yank-at-point             t)

(add-hook 'emacs-startup-hook (lambda () (toggle-frame-maximized)))
(when *is-a-mac* (setq ns-use-native-fullscreen nil))

(blink-cursor-mode 0)

(setq uniquify-buffer-name-style 'forward) ; unique buffer names

(save-place-mode 1); https://www.emacswiki.org/emacs/SavePlace

(show-paren-mode 1)                        ; Indicate matching pairs of
                                           ; parentheses
(setq show-paren-delay 0.0)

(setq-default indent-tabs-mode nil)
(setq tab-width 2) ; I like tab-width 2. Note that certain languages may need
                   ; different tab-width

(setq-default fill-column 80) ; always break at 80
(column-number-mode 1)

(setq delete-selection-mode     t
      sentence-end-double-space nil
      vc-follow-symlinks        t
      default-directory         "~/"
      confirm-kill-emacs        'y-or-n-p
      require-final-newline     t)
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode t)   ; is this really a good idea?
(global-auto-revert-mode t) ; refresh buffer on file change

(setq visible-bell t) ; visual DING!

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(setq large-file-warning-threshold (* 15 1024 1024))

(set-face-attribute 'default nil        :family "IBM Plex Mono" :height 180)
(set-face-attribute 'fixed-pitch nil    :family "IBM Plex Mono")
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans")
                                        ; IBM Plex Serif is also good for
                                        ; variable-pitch

(defun knube/fix-org-blocks ()
  (interactive)
  (eval-after-load 'org
    (lambda ()
      (set-face-attribute 'org-block nil :extend t)
      (set-face-attribute 'org-block-begin-line nil :extend t)
      (set-face-attribute 'org-block-end-line nil :extend t))))

(use-package modus-operandi-theme) ; light theme
(use-package modus-vivendi-theme)  ; dark theme

(defun knube/toggle-theme ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

(setq modus-operandi-theme-mode-line nil 
      modus-vivendi-theme-mode-line  nil)

(setq modus-operandi-theme-org-blocks 'greyscale
      modus-vivendi-theme-org-blocks  'greyscale)


(setq modus-operandi-theme-variable-pitch-headings t
      modus-vivendi-theme-variable-pitch-headings  t)

(setq modus-operandi-theme-scale-headings t
      modus-vivendi-theme-scale-headings  t)

(setq modus-operandi-theme-scale-1 1.2
      modus-operandi-theme-scale-2 1.4
      modus-operandi-theme-scale-3 1.6
      modus-operandi-theme-scale-4 1.8
      modus-operandi-theme-scale-5 2.0)

(setq modus-vivendi-theme-scale-1 1.2
      modus-vivendi-theme-scale-2 1.4
      modus-vivendi-theme-scale-3 1.6
      modus-vivendi-theme-scale-4 1.8
      modus-vivendi-theme-scale-5 2.0)

(load-theme 'modus-operandi t)

(use-package minions
  :config
  (setq minions-mode-line-lighter    "☰"
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package telephone-line
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment
                     telephone-line-airline-position-segment))
          (accent . (telephone-line-buffer-name-segment))
          (nil    . (telephone-line-buffer-modified-segment)))
        
        telephone-line-rhs
        '((nil    . (telephone-line-minions-mode-segment))
          (accent . (telephone-line-vc-segment))
          (nil    . (telephone-line-misc-info-segment))))

  (unless (equal "Battery status not available"
                 (battery))          
    (display-battery-mode 1))      
  
  (setq display-time-24hr-format            t  
        display-time-day-and-date           t
        display-time-default-load-average   nil
        display-time-load-average           nil
        display-time-load-average-threshold nil)
  (display-time-mode 1)             
  
  (telephone-line-mode 1))

(use-package writeroom-mode)

(use-package general)

(general-unbind "M-v"      ; I need to embrace evil's copy and paste
                "M-c"
                "s-p"      ; no one needs print
                "C-x f"    ; set-fill-column is always 80
                "C-x C-n") ; set-goal-column is just annoying

(use-package which-key
  :config
  (setq which-key-idle-delay 0.3
        which-key-separator " "
        which-key-prefix-prefix "+")
  (which-key-mode))

(use-package evil
  :after helm 
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil
        evil-want-fine-undo   t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-keybinding nil)
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))

(use-package helm
  :init
  (require 'helm-config)
  :config
  (setq helm-autoresize-max-height 33  ; these are percentages?
        helm-autoresize-min-height 33)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  
  (general-define-key
   "M-x"     'helm-M-x
   "C-x C-f" 'helm-find-files
   "C-x b"   'helm-mini)
  
  (general-define-key
   :keymaps 'helm-map
   "<tab>" 'helm-execute-persistent-action
   "C-i"   'helm-execute-persistent-action
   "C-z"   'helm-select-action))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-org)

(use-package helm-swoop
  :config
  (general-define-key
   "M-i"     'helm-swoop
   "M-I"     'helm-swoop-back-to-last-point
   "C-c M-i" 'helm-multi-swoop
   "C-x M-i" 'helm-multi-swoop-all)
  
  (general-define-key
   :keymaps 'isearch-mode-map
   "M-i" 'helm-swoop-from-isearch) ; from isearch

  (general-define-key
   :keymaps 'evil-motion-state-map
   "M-i" 'helm-swoop-from-evil-search) ; from evil search
  
  (general-define-key
   :keymaps 'helm-swoop-map
   "M-i" 'helm-multi-swoop-all-from-helm-swoop          ; from helm-swoop to helm-multi-swoop
   "M-m" 'helm-multi-swoop-current-mode-from-helm-swoop
   "C-r" 'helm-previous-line                            ; move up and down like isearch
   "C-s" 'helm-next-line)

  (general-define-key
   :keymaps 'helm-multi-swoop-map
   "C-r" 'helm-previous-line
   "C-s" 'helm-next-line)

  (setq helm-multi-swoop-edit-save             t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction             'split-window-vertically
        helm-swoop-speed-or-color              nil
        helm-swoop-move-to-line-cycle          t
        helm-swoop-use-line-number-face        t
        helm-swoop-use-fuzzy-match             t))

(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay            0.25
        company-minimum-prefix-length 2
        company-tooltip-limit         15))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . variable-pitch-mode)
  :config
  (require 'org-tempo)
  ;; (require 'ob-latex)
  ;; (require 'ob-emacs-lisp)
  (add-hook 'org-mode-hook     'turn-on-org-cdlatex)
  (add-hook 'org-src-mode-hook 'turn-on-org-cdlatex) 
 
  (setq org-list-allow-alphabetical      t
        org-startup-indented             nil   ; indent sections
        org-indent-indentation-per-level 0 
        org-adapt-indentation            nil
        org-src-tab-acts-natively        t     ; tab works as in any major mode
        org-src-preserve-indentation     t
        org-log-into-drawer              t     ; wtf is this?
        org-src-fontify-natively         t     ; highlight code
        org-log-done                     'time ; add dates on completion of
                                               ; TODOs
        org-support-shift-select         t     ; select holding down shift
        org-startup-truncated            nil
        org-directory                    "~/Dropbox/org"
        org-agenda-files                 '("~/Dropbox/org/agenda")
        org-ellipsis                     "⤵"
        org-src-window-setup             'current-window
        org-latex-pdf-process            (list "latexmk -f %f"))
  
  (add-to-list 'org-structure-template-alist '("se" . "src emacs-lisp"))
  (general-unbind
    :keymaps 'org-mode-map
    "C-c '"  ; redefined below
    "C-c [") ; I have no need to "put whatever to the front of the agenda"
  (general-define-key
   :keymaps 'org-mode-map
   "C-c C-'" 'org-edit-special)
  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-'" 'org-edit-src-exit))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-ref
  :after org
  :config
  (setq reftex-default-bibliography '("~/Dropbox/org/bibliography/references.bib")) ;; move this?

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes   "~/Dropbox/org/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/org/bibliography/references.bib")
        org-ref-pdf-directory        "~/Dropbox/org/bibliography/bibtex-pdfs/"))

(use-package tex-site
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-PDF-mode)

  :config
  (setq-default TeX-master nil
                TeX-engine 'xetex)
  
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate        t
        ; TeX-PDF-mode                t
        TeX-auto-save               t
        TeX-save-query              nil
        TeX-parse-self              t
        reftex-plug-into-AUCTeX     t
        TeX-view-program-list       '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))
        TeX-view-program-selection  '((output-pdf "Skim"))
        TeX-clean-confirm           nil))

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package cdlatex
  :hook
  (org-mode   . turn-on-org-cdlatex)
  (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-env-alist
        '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}\n" nil))))

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package company-math
  :config
  (add-to-list 'company-backends '(company-math-symbols-latex
                                   company-latex-commands)))

(use-package evil-tex
  :hook
  (LaTeX-mode . evil-tex))
