;;; early-init.el --- -*- lexical-binding: t -*-
(defvar startup-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold         most-positive-fixnum ; set to 32MB later
      gc-cons-percentage        0.6                  ; set to 0.1 later
      debug-on-error            t                    ; reset later
      file-name-handler-alist   nil                  ; reset later
      site-run-file             nil
      package-enable-at-startup nil)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-message t
      inhibit-scratch-message t
      load-prefer-newer       t)

(setq-default evil-want-keybinding nil)
