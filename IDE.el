;; set color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue2)
;; enable auto-complete
(global-auto-complete-mode t)

(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

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

;; Modified settings from RealPython

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
    ein
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
;;(elpy-use-ipython) ;; deprecated use jupyter instead
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
