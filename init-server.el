;; My Emacs configuration file for remote servers.

;; Add MELPA to package archive list.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Don't show a startup message.
(setq inhibit-startup-message t)

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show syntax highlighting
(global-font-lock-mode t)

;; Highlight marked regions
(setq-default transient-mark-mode t)

;; Parentheses
(electric-pair-mode 1)                  ; automatically close parentheses, etc.
(show-paren-mode t)                     ; show matching parentheses

;; Smooth scrolling (one line at a time)
(setq scroll-step 1)

;; Tab settings: 4 spaces.
(setq tab-width 4)

;; Check if the use-package package is installed and install it if not.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Intellisense syntax checking
(use-package flycheck
  :init (global-flycheck-mode)

  ;; C++
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))

  ;; eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  :ensure t
  :defer t
)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(flycheck use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable auto-backup files.
(setq make-backup-files nil)

;; Disable auto-save files.
(setq auto-save-default nil)

;; Global company mode that allows dynamic autocomplete.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable elpy for Python development.
(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))
(elpy-enable)

;; Color theme.
(unless (package-installed-p 'darcula-theme)
  (package-refresh-contents)
  (package-install 'darcula-theme))
(load-theme 'darcula t)

;; Show line numbers and disable the fringe.
(global-display-line-numbers-mode)
(fringe-mode 0)

;; Customize highlighting of TODO keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(TODO\\)\\>" 1 'my-todo-face t)))))
(defface my-todo-face
  '((t (:foreground "cyan" :weight bold)))
  "Face for highlighting TODO keywords.")

;; Customize highlighting of WARNING keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(WARNING\\)\\>" 1 'my-warning-face t)))))
(defface my-warning-face
  '((t (:foreground "black" :background "red" :weight bold)))
  "Face for highlighting WARNING keywords.")

;; Customize highlighting of NOTE keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(NOTE\\)\\>" 1 'my-note-face t)))))
(defface my-note-face
  '((t (:foreground "yellow")))
  "Face for highlighting NOTE keywords.")

;; Customize highlighting of INFO keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(INFO\\)\\>" 1 'my-info-face t)))))
(defface my-info-face
  '((t (:foreground "green")))
  "Face for highlighting INFO keywords.")

;; Enable hs-minor-mode to fold code blocks.
(add-hook 'prog-mode-hook #'hs-minor-mode)
