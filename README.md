early-init.el

```
;;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
```

init.el

```
;;; -*- lexical-binding: t; -*-

;; --- straight.el bootstrap ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight for use-package declarations
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;; --- Evil mode (Vim keybindings) ---
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; --- Alloy major mode (.als) ---
(use-package alloy-mode
  :straight (alloy-mode :type git :host github :repo "dwwmmn/alloy-mode")
  :mode "\\.als\\'"
  :init
  (setq alloy-basic-offset 2))

;; --- LSP client ---
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-to-list 'lsp-language-id-configuration '(alloy-mode . "alloy"))
  (add-to-list 'lsp-language-id-configuration '(markdown-mode . "markdown")))

(add-to-list 'load-path (expand-file-name "~/alloy-lsp"))
(autoload 'alloy-lsp-ensure "alloy-lsp" nil t)
(add-hook 'alloy-mode-hook #'alloy-lsp-ensure)
```
