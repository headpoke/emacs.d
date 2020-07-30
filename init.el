;;; -*- lexical-binding: t; -*-

;; Ensure that the tangled file uses lexical scoping
(setq-default lexical-binding t)

;; Tangling requires org
(require 'org)
;; Find config files
(find-file (concat user-emacs-directory "private.org"))
(find-file (concat user-emacs-directory "knube.org"))
;; Tangle them
(org-babel-tangle)
;; Load
(load-file (concat user-emacs-directory "private.el"))
(load-file (concat user-emacs-directory "knube.el"))


;; (byte-compile-file (concat user-emacs-directory "private.el"))
;; (byte-compile-file (concat user-emacs-directory "knube.el"))
