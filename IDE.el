;; INSTALL PACKAGES
;; --------------------------------------

;; Enable package stuff
;; load melpa for package installing
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

       
(defvar myPackages
  '(better-defaults
    async
    ein
    elpy
    flycheck
    material-theme
    realgud 
    python-mode 
    auto-complete
    yasnippet
    yasnippet-snippets
    magit
    py-autopep8
    dockerfile-mode
    docker-compose-mode
    org
    ))

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; recent files entry
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; prefer utf-8 as encoding
(prefer-coding-system 'utf-8)

;; set color theme
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-dark-blue2)
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

;; Key Bindings
;;(with-eval-after-load 'python-mode
(define-key global-map (kbd "s-d") 'comment-region) ;; comment a region by shortcut Super+d
(define-key global-map (kbd "s-D") 'uncomment-region) ;; uncomment a region by shortcut Super+Shift+d
(define-key global-map (kbd "C-r") 'query-replace)    ;; replace
(define-key global-map (kbd "C-f") 'isearch-forward)   ;; search
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward) ;; search forward
(define-key global-map (kbd "C-s") 'isearch-backward)  ;; search backward
(define-key isearch-mode-map "\C-s" 'isearch-repeat-backward) ;; repeat backward
(define-key global-map (kbd "C-M-f") 'search-forward-regexp)   ;; search
(define-key global-map (kbd "C-M-s") 'search-backward-regexp)  ;; search backward
(define-key global-map (kbd "C-a") 'mark-whole-buffer)  ;; select all
(define-key global-map (kbd "C-x C-o") 'find-file)  ;; open file

(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
     ad-do-it))

;;  )
(define-key global-map [menu-bar devtools comment] '("Comment Region" . comment-region))
(define-key global-map [menu-bar devtools uncomment] '("Unomment Region" . uncomment-region))

;; Python Debugging
(define-key global-map (kbd "s-r") 'realgud:ipdb)  ;; start realgud
(define-key global-map (kbd "s-e") 'realgud-short-key-mode)  ;; start realgud

(define-key global-map [menu-bar devtools start-ipdb] '("Start ipdb" . realgud:ipdb))
(define-key global-map [menu-bar devtools ipdb-debug] '("Debugs Symbols" . realgud-short-key-mode))

;; buffer switching
(define-key global-map (kbd "s-<right>") 'next-buffer)  ;; switch to next buffer
(define-key global-map (kbd "s-<left>") 'previous-buffer)  ;; switch to previous buffer

;;)
;;(require 'ido
;;(ido-mode t)
(require 'python-mode)
;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; add scons file to the known python file list
(add-to-list 'auto-mode-alist '("\\SConstruct\\'" . python-mode))

;; Normal Copy paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Load yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Modified settings from RealPython
;; BASIC CUSTOMIZATION
;; --------------------------------------

;;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
;;(elpy-use-ipython) ;; deprecated use other instead
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
;; python mode settings
(global-eldoc-mode -1)
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

             
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; load Realgud
(load-library "realgud")

;; start server
(server-start)
