;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; ---------------------------------------
;; General Configuration changes
;; ---------------------------------------


;; ---------------------------------------
;; Configuration:
;; ---------------------------------------
(setq config '(
               (:buffer-search-tool . swiper)))

;; ---------------------------------------
;; Text setup
;; ---------------------------------------
(setq-default line-spacing 0.25)
;; ---------------------------------------

;; ---------------------------------------
;; Line numbers
;; native line numbers taking up lots of space?
(setq-default display-line-numbers-width nil)
;; ---------------------------------------

;; ---------------------------------------
;; Lsp
;; ---------------------------------------
;; Disable by default. Enable only by keymap
;; enable-flyspell-auto-completion nil
;; spell-checking-enable-by-default nil
(defun etlk/lsp-ui-doc-handle ()
  "Wrapper function which shows symbol `lsp-ui-doc` popup if not showed.
  If doc popup is showed, then focus doc"
  (interactive)
  (cond
   ((lsp-ui-doc--frame-visible-p) (lsp-ui-doc-focus-frame))
   ((not (lsp-ui-doc--frame-visible-p)) (lsp-ui-doc-show))))

;FIXME: doesnt work keymap. emacs used default
(with-eval-after-load 'lsp-ui
  (global-set-key (kbd "K") #'etlk/lsp-ui-doc-handle))

;; hack to make typing snappier in typescript
(defun etlk/typescript-mode-make-snappier-input ()
  (fset #'jsonrpc--log-event #'ignore))

(add-hook 'typescript-mode-hook #'etlk/typescript-mode-make-snappier-input)

(setq eldoc-idle-delay 0.75)
(setq flymake-no-changes-timeout 0.5)

;; ---------------------------------------

;; ---------------------------------------
;; Node modules resolution
;; ---------------------------------------
(dolist (mode '(
                js-mode
                  typescript-mode))
  (dolist (hook '(js-mode-hook typescript-mode-hook))
    (with-eval-after-load 'mode
      '(add-hook 'hook #'add-node-modules-path))))

;; (with-eval-after-load 'js-mode
;;   '(add-hook 'js-mode-hook #'add-node-modules-path))

;; monorepo specific
(setq add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))
;; ---------------------------------------

;; ---------------------------------------
;; Telegram chat
(setq-default telega-use-docker t)

(let ((telega-dir (file-name-directory (locate-library "telega"))))
  (push
   (expand-file-name "contrib" (directory-file-name (file-name-directory telega-dir)))
   load-path))

;; Dired for the file attaching
(require 'telega-dired-dwim)

;; Code blocks highlighting
(require 'telega-mnz)
(add-hook 'telega-load-hook 'global-telega-mnz-mode)
;; Notifications
(add-hook 'telega-load-hook 'telega-appindicator-mode)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(telega-notifications-mode 1)
;; Autocompletion for the chat
;; (add-hook 'telega-chat-mode-hook 'company-mode)

(require 'telega-url-shorten)
(global-telega-url-shorten-mode 1)
;; Dashboard
;; (require 'telega-dashboard)
;; (add-to-list 'dashboard-items '(telega-chats . 5))
;; ---------------------------------------

;; ---------------------------------------
;; Terminal
;; (require 'terminal-here)
;; (setq terminal-here-terminal-command '("wezterm"))
;; (setq terminal-here-verbose t)

;; (defun terminal-here-lazygit ()
;;   "Open a terminal in the project directory and launch lazygit."
;;   (interactive)
;;   (require 'terminal-here)
;;   (terminal-here-launch (list "lazygit"))
;;   (message terminal-here-terminal-command))


;; (global-set-key (kbd "C-<f7>") #'terminal-here-lazygit)
;; ---------------------------------------

;; ---------------------------------------
;; Ivy extensions
;; ---------------------------------------
(with-eval-after-load 'ivy
  (setq ivy-posframe-display-functions-alist
        '((counsel-M-x     . ivy-posframe-display-at-frame-center)
          (xref            . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))
;; ---------------------------------------

;; ---------------------------------------
;; Solaire-mode setup
;; ---------------------------------------
(with-eval-after-load 'solaire-mode
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))
(with-eval-after-load 'vterm-mode
  (#'turn-on-solaire-mode))
(with-eval-after-load 'eshell-mode
  (#'turn-on-solaire-mode))
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;; ---------------------------------------

;; add https://github.com/lorniu/go-translate
;; ---------------------------------------
;; Searching
(evil-global-set-key 'normal "/" (alist-get :buffer-search-tool config))
;; ---------------------------------------


;; ---------------------------------------
;; Git
;; ---------------------------------------
(spacemacs/declare-prefix "gw" "Worktrees")
(spacemacs/set-leader-keys
  "gw."   '("Menu" . magit-worktree)
  "gwb" '("Create with branch" . magit-worktree-branch)
  "gwm" '("Move" . magit-worktree-move)
  "gwd" '("Delete" . magit-worktree-delete)
  "gwc" '("Checkout" . magit-worktree-checkout))

;; ---------------------------------------
;; Helm Descbinds
;; Recent release of helm-descbinds package breaks which-key menu
;; Remove helm-discbinds-mode from helm mode hook to avoid activating
;; https://github.com/syl20bnr/spacemacs/issues/16276
(remove-hook 'helm-mode-hook 'helm-descbinds-mode)
;; ---------------------------------------

; LocalWords:  gw
