;;; My Emacs configuration file.
;;;
;;; Thanks to Andrew DeOrio -
;;; his entire init.el here:
;;; https://github.com/awdeorio/dotfiles/blob/master/.emacs.d/init.el

;; Add MELPA to package archive list.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Modified keyboard shortcuts
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\M-o"                              'other-window)
(global-set-key "\C-x\C-o"                          'other-frame)
(global-set-key (kbd "C-c r")                       'revert-buffer)

;; Don't show a startup message
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

;; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

;; Set window width to 80 characters.
(defun set-window-width ()
  "Set the width of the window to 85 characters."
  (interactive)
  (if (window-system)
      (set-frame-width (selected-frame) 85)))
(add-hook 'window-configuration-change-hook 'set-window-width)

;; Disable auto-backup files.
(setq make-backup-files nil)

;; Disable auto-save files.
(setq auto-save-default nil)

;; Adjust window height.
(setq default-frame-alist
      '((height . 50)))

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

;; git.
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

;; View directory tree with neotree.
(use-package neotree
  :ensure t
  :bind ("C-c n" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(use-package all-the-icons
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-preview-mode markdown-mode company-lsp web-mode company-tern darcula-theme dakrone-theme hc-zenburn-theme zenburn-theme color-theme-modern all-the-icons use-package undo-tree spacemacs-theme realgud-lldb one-themes neotree monokai-pro-theme magit flycheck elpy auto-complete atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Web Development
(use-package web-mode
  :mode "\\.jsx?\\'"
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :ensure t
  :defer t
  )

;; Enable hs-minor-mode to fold code blocks.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Add IN-PROGRESS keyword to org mode.
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "red")))

;; Markdown mode.
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Shortcut to preview a markdown file.
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c p") 'markdown-preview)))

;; Latex tools.
;; (use-package auctex
;;   :ensure t)
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; Set pdf tools as the default viewer.
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
      TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

;; Automatically refresh the viewer.
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; Compile on save.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (TeX-command "LaTeX" 'TeX-master-file -1)) nil 'local)))
