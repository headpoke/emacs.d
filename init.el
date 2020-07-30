;;; -*- lexical-binding: t; -*-

;; Ensure that the tangled file uses lexical scoping
(setq-default lexical-binding t)

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
