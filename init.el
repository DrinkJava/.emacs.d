;;; init.el --- Emacs init file
;;  Author: Nishant Maniam
;;; Commentary:
;;; A lightweight Emacs config containing only the essentials: shipped with a custom theme!
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6
;;       file-name-handler-alist nil
;;       site-run-file nil)

;; (defvar nish/gc-cons-threshold 20000000)

;; (add-hook 'emacs-startup-hook ; hook run after loading init files
;;           (lambda ()
;;             (setq gc-cons-threshold nish/gc-cons-threshold
;;                   gc-cons-percentage 0.1
;;                   file-name-handler-alist file-name-handler-alist-original)))

;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (setq gc-cons-threshold (* nish/gc-cons-threshold 2))))
;; (add-hook 'minibuffer-exit-hook (lambda ()
;;                                   (garbage-collect)
;;                                   (setq gc-cons-threshold nish/gc-cons-threshold)))

;; (require 'package)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
;; (setq package-enable-at-startup nil)
;; (package-initialize)

;; straight.el https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (unless (package-installed-p 'use-package)
;;   (straight-use-package 'use-package))

;; (setq straight-use-package-by-default t)
;; (setq straight-built-in-pseudo-packages t)


;; Load main config file "./config.org"
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init)
;;; init.el ends here
