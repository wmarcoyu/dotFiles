;;; package --- Summary My Emacs configuration file.
;;;
;;; Thanks to Andrew DeOrio -
;;; his entire init.el here:
;;; https://github.com/awdeorio/dotfiles/blob/master/.emacs.d/init.el

;; Add MELPA to package archive list.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

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

;; Let's be a little transparent (on macOS).
( when (eq system-type 'darwin)
    (defun set-frame-transparency (&optional frame)
      (let ((alpha '(90 . 90)))
	(when frame
	  (set-frame-parameter frame 'alpha alpha))
	(add-to-list 'default-frame-alist `(alpha . ,alpha))))
  ;; Apply transparency to the current frame
  (set-frame-transparency (selected-frame))
  ;; Ensure all future frames get the same transparency
  (add-hook 'after-make-frame-functions 'set-frame-transparency)
 )

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

;; Highlight characters beyond 90 columns.
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 90)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Disable auto-backup files.
(setq make-backup-files nil)

;; Disable auto-save files.
(setq auto-save-default nil)

;; Adjust window width and height.
(setq default-frame-alist
      '((width . 90) (height . 55)))

;; Global company mode that allows dynamic autocomplete.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable elpy for Python development.
(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))
(elpy-enable)

;; C++ autocomplete.
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (c++-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(setq-default c-basic-offset 2)

;; Color theme.
(unless (package-installed-p 'darcula-theme)
  (package-refresh-contents)
  (package-install 'darcula-theme))
(load-theme 'darcula t)

;; (use-package spacemacs-common
;;     :ensure spacemacs-theme
;;     :defer t
;;     )
;; (load-theme 'spacemacs-dark t)

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
  :defer t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

;; View directory tree with neotree.
(use-package neotree
  :ensure t
  :defer t
  :bind ("C-c n" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(use-package all-the-icons
  :ensure t
  :defer t
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(prettier-js typescript-mode vue-mode yaml-mode load-relative loc-changes test-simple realgud cargo rust-mode kotlin-mode lsp-java auto-complete-auctex auto-comlete-auctex lsp-ui lsp-mode markdown-preview-mode markdown-mode company-lsp web-mode company-tern darcula-theme dakrone-theme hc-zenburn-theme zenburn-theme color-theme-modern all-the-icons use-package undo-tree spacemacs-theme realgud-lldb one-themes neotree monokai-pro-theme magit flycheck elpy auto-complete atom-one-dark-theme)))
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
  :mode "\\.vue\\'"
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

(use-package markdown-preview-mode
  :ensure t
  :defer t
)

(add-hook 'markdown-mode-hook 'markdown-preview-mode)
(setq markdown-preview-stylesheets (list ""))

;; Latex tools.
(use-package tex
  :ensure auctex
  :defer t
)

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

;; Java Intellisense.
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook (java-mode . lsp-deferred))

(use-package lsp-java
  :ensure t
  :defer t
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

;; Rust Development.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'rust-mode)
  (package-refresh-contents)
  (package-install 'rust-mode))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(unless (package-installed-p 'cargo)
  (package-refresh-contents)
  (package-install 'cargo))
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; Flycheck for Rust.
(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))
(add-hook 'rust-mode-hook 'flycheck-mode)

;; Remote editing with Tramp.
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlMaster auto "
         "-o ControlPersist yes "
         "-o ControlPath ~/.ssh/socket-%%C "
         "-o ServerAliveInterval 60 "
         "-o ServerAliveCountMax 5 "
         ))
  (setq tramp-use-ssh-controlmaster-options nil)
  :defer t
)

;; GitHub Copilot requirements.
(use-package dash
  :ensure t
  :defer t
)

(use-package s
  :ensure t
  :defer t
)

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package f
  :ensure t
  :defer t
)
;; GitHub Copilot.
(add-to-list 'load-path "~/.emacs.d/copilot.el")
(require 'copilot)
;; you can utilize :map :hook and :config to customize copilot
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "\C-x\C-y") 'copilot-accept-completion)
(setq copilot-indent-offset-warning-disable t)

;; Run Python code in org-mode.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t))) ;; Make sure Python is enabled
(setq org-babel-python-command "python3")

;; YAML mode.
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t
  :defer t
)

;; Vue mode.
(use-package vue-mode
  :ensure t
  :defer t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2)
              (setq css-indent-offset 2)
              (setq tab-width 2)))
)

;; TypeScript mode.
(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
)

;; LSP for Vue and TypeScript.
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((vue-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp
)
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
)

;; Prettier for Vue and TypeScript.
(use-package prettier-js
  :ensure t
  :defer t
  :hook ((vue-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
)
