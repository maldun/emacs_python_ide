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
    popwin
    auto-complete
    yasnippet
    yasnippet-snippets
    magit
    py-autopep8
    dockerfile-mode
    docker-compose-mode
    org
    markdown-mode
    neotree   
    all-the-icons
    use-package
    highlight-symbol
    blacken
    color-theme-modern
    diff-hl
    bm
    highlight-numbers
    auto-highlight-symbol
    ))

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)
(setq org-time-stamp-custom-formats '("<%d/%m/%y %a %H:%M:%S>" "<%d/%m/%y %a %H:%M:%S>"))
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
;; (global-auto-complete-mode t)

;; (defun auto-complete-mode-maybe ()
;;   "No maybe for you. Only AC!"
;;   (unless (minibufferp (current-buffer))
;;     (auto-complete-mode 1)))
(ac-config-default)

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
(define-key global-map [menu-bar devtools uncomment] '("Uncomment Region" . uncomment-region))

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
(global-linum-mode 0) ;; enable line numbers globally ... slow therefore disabled
;; alternative from http://ergoemacs.org/emacs/emacs_line_number_mode.html:
(defun nolinum ()
  (global-linum-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)
(add-hook 'python-mode-hook 'nolinum)
(global-display-line-numbers-mode t) ;; enable line numbers 
;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(setq py-interpreter "python3")
(setq elpy-rpc-python-command py-interpreter)
(message "Install Python packages")
(shell-command (mapconcat 'identity (list py-interpreter "-m" "pip" "install" "--user" "jedi" "autopep8" "yapf" "black" "flake8" "importmagic") " "))
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

(message "Link snippets")
(setq snip1 "defs")
(setq snip2 "dph")

(setq middle "elpa/elpy-*/snippets/python-mode/")
(setq path-helper1 (mapconcat 'identity (list IDE-path middle) ""))
(setq path-helper2 (mapconcat 'identity (list IDE-path snip1) ""))
(shell-command (mapconcat 'identity (list "ln" "-sf" path-helper2 path-helper1) " "))

(setq path-helper3 (mapconcat 'identity (list IDE-path middle) ""))
(setq path-helper4 (mapconcat 'identity (list IDE-path snip2) ""))
(shell-command (mapconcat 'identity (list "ln" "-sf" path-helper4 path-helper3) " "))


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

;; Show Brackets
(show-paren-mode 1)

;; A static file list is really cool, like a modern IDE
(package-initialize)
(require 'package)
(require 'neotree)
(global-set-key (kbd "C-x M-f") 'neotree-toggle)
  (setq neo-window-fixed-size nil)

;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(desktop-save-mode 1)

;; autocomplete at start
(global-auto-complete-mode t)

;; Showing connected parentheses 
(show-paren-mode 1)

;; You require use-package to...
(require 'use-package)

;; .. run this command only once
;; It will install the fonts only on first run
(use-package all-the-icons
  :ensure t
  :config
  :init
  (unless (package-installed-p 'all-the-icons)
    (all-the-icons-install-fonts))
  )

;; All modern IDEs have the feature to obtain a list of the current documents
(require 'all-the-icons)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Showing differences to the last Git commit
(require 'diff-hl)
(global-diff-hl-mode)

;; As your cursor is on a variable, all other occurrances will lighten up
(require 'highlight-symbol)
(highlight-symbol-mode 1)

;; On Ctrl-Left-Click goto definition
;; Pretty useful, proudly stolen from https://emacs.stackexchange.com/a/19194/21383
(defun goto-def-or-rgrep ()
  "Go to definition of thing at point or do an rgrep in project if that fails"
  (interactive)
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))
(define-key elpy-mode-map [C-down-mouse-1]  'goto-def-or-rgrep)

;; I like to have a shell running in a window, yet it keeps being overwritten
;; C-c t now dedicates the window to the buffer
;; Proudly stolen from https://emacs.stackexchange.com/a/2198/21383
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;;Marking lines
(require 'bm)
(global-set-key (kbd "<s-f2>") 'bm-toggle)
(global-set-key (kbd "<s-f4>")   'bm-next)
(global-set-key (kbd "<s-f3>") 'bm-previous)
;;Change the colour if you don't like RoyalBlue4
(set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified)

;; Emacs 24 renamed flet to cl-flet.
(defalias 'mh-cl-flet
  (if (fboundp 'cl-flet)
      'cl-flet
    'flet))

;;{{{ yasnippet & auto-insert

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

(defun marioschwaiger/yas-python ()
  (interactive)
;;  (yas-expand-snippet (yas-lookup-snippet "cmake_minimum_required" 'cmake-mode)))
  (yas-expand-snippet (yas-lookup-snippet "do_python_header" 'python-mode)))


(use-package autoinsert
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-alist nil) ;; remove this like to restore defaults
;;  (add-to-list 'auto-insert-alist  '("CMakeLists\\.txt$" . [nega/yas-cmake-bp])))
;;  (add-to-list 'auto-mode-alist '("\\.py\\'"  . python-mode)))
  (add-to-list 'auto-insert-alist  '("\\.py$" . [marioschwaiger/yas-python])))

;;}}}

;; automatically reverts buffers
(global-auto-revert-mode 1)

;; highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; load color theme (optional)
;; (load-theme 'dark-blue2)

;; highlight-symbols: But works!
(global-auto-highlight-symbol-mode)
(define-key auto-highlight-symbol-mode-map (kbd "M-p") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "M-n") 'ahs-forward)
(setq ahs-idle-interval 1.0) ;; if you want instant highlighting, set it to 0, but I find it annoying
(setq ahs-default-range 'ahs-range-whole-buffer) ;; highlight every occurence in buffer

;; inhibits highlighting in specific places, like in comments
(setq ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                                font-lock-comment-face
                                font-lock-doc-face
                                font-lock-doc-string-face
                                font-lock-string-face))


;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


(require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)

(sublimity-mode 1)

(setq sublimity-scroll-weight 10
      isublimity-scroll-drift-length 5)

;; Highlights current line
(global-hl-line-mode +1)
