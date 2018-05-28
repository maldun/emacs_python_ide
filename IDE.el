;; Creating a new menu pane in the menu bar to the right of “Tools” menu
(define-key-after
  global-map
  [menu-bar devtools]
  (cons "DevTools" (make-sparse-keymap "Bla"))
  'tools )

;; won't work: (kept for reference)
;;(with-eval-after-load 'python-mode
(define-key global-map (kbd "s-d") 'comment-region) ;; comment a region by shortcut Super+d
(define-key global-map (kbd "s-D") 'uncomment-region) ;; uncomment a region by shortcut Super+Shift+d
(define-key global-map (kbd "C-r") 'query-replace)    ;; replace
(define-key global-map (kbd "C-f") 'search-forward)   ;; search

;;  )
(define-key global-map [menu-bar devtools comment] '("Comment Region" . comment-region))
(define-key global-map [menu-bar devtools uncomment] '("Unomment Region" . uncomment-region))

;;)
;;(require 'ido
;;(ido-mode t)
(require 'python-mode)
;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-to-list 'auto-mode-alist '("\\SConstruct\\'" . python-mode))

;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; init.el ends here
