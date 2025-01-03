;;; init.el --- Emacs init file
;;  Author: Nishant Maniam

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold 1000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq package-enable-at-startup nil)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq default-directory "~/")

;; Load main config file "./config.org"
(straight-use-package 'org)
(setq org-agenda-files '("~/nish-roam/daily/" "~/nish-roam/"))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
 
(provide 'init)
;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f3781be0be23cc71c89b317489e07a4ad3e885f84c0a618692b53bbd69e60843" "1896e14316f5f9bb073e0cd43fa56993a15b751c02d771114f595b7b18c4ba04" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" default))
 '(org-confirm-babel-evaluate nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
