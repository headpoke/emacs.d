;; -*- lexical-binding: t -*-

;; Welcome to knubemacs, enjoy your stay!

;; Version and OS checks
(when (version< emacs-version "26.3")
  (warn "Your Emacs might be too old -- This setup was made for version 26.3
          or newer. There are various bugs for 26.2 and earlier, and I have not
          implemented any workarounds. Proceed with caution!"))

(defconst *is-a-mac* (eq system-type 'darwin))

(when (not *is-a-mac*)
  (warn "This config was made for my Macbook Air and I have no
         idea how it will work on different platforms. Proceed
         with caution."))

(setq load-prefer-newer t)

;; Increase the threshold for garbage collection during emacs-startup,
;; decrease to 32 mb at end of startup. Also enable debugger during
;; startup. The debug-part can be set to nil once your emacs config is
;; working perfectly.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      debug-on-error t)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024)
                           gc-cons-percentage 0.1
                           debug-on-error nil)))

;; save custom settings in custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


;; use-package
(require 'package)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
			 ("elpa"         . "https://elpa.gnu.org/packages/")
			 ("org"          . "https://orgmode.org/elpa/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package)
                   (setq use-package-always-ensure t))

;; https://github.com/emacscollective/auto-compile
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defun knube/tangle-and-reload-config()
  (interactive)
  (let ((src (concat user-emacs-directory "knube.org"))
        (dst (concat user-emacs-directory "knube.el")))
    (when (file-newer-than-file-p src dst)
      (call-process
       (concat invocation-directory invocation-name)
       nil nil t
       "-q" "--batch" "--eval" "(require 'ob-tangle)"
       "--eval" (format "(org-babel-tangle-file \"%s\" \"%s\")" src dst)))
  ;; I don't think bytecompiling the config files is a good idea
  ;; (require 'bytecomp)
  ;; (byte-recompile-file dst nil 0 t)
    (load-file dst)))

(knube/tangle-and-reload-config)

(provide 'init)
