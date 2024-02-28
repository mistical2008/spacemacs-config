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
;; Line numbers
;; native line numbers taking up lots of space?
(setq-default display-line-numbers-width nil)
;; ---------------------------------------

;; ---------------------------------------
;; Telegram chat
(setq-default telega-use-docker t)
(add-hook 'telega-load-hook 'global-telega-mnz-mode)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(with-eval-after-load 'telega-use-docker
  (telega-alert-mode 1))
;; ---------------------------------------

;; Ivy extensions
(with-eval-after-load 'ivy
  (setq ivy-posframe-display-functions-alist
        '((counsel-M-x     . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

;; Solaire-mode setup
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
;; Searching
(evil-global-set-key 'normal "/" (alist-get :buffer-search-tool config))
(evil-global-set-key 'normal "<leader>gwb" magit-worktree-branch)
;; "<leader>gwm" magit-worktree-move
;; "<leader>gwd" magit-worktree-delete
;; "<leader>gwc" magit-worktree-checkout
;; "<leader>gws" magit-worktree-status

;; (defun my-magit-worktrees-keybindings ()
;;   "Define keybindings for Magit worktrees."
;;   (which-key-add-key-based-replacements
;;     "<SPC> g w" "Magit worktrees"
;;     "<SPC> g w b" "Create new worktree"
;;     "<SPC> g w m" "Move worktree"
;;     "<SPC> g w d" "Delete worktree"
;;     "<SPC> g w s" "Show worktree status"
;;     "<SPC> g w c" "Checkout worktree")
;;   (spacemacs/set-leader-keys-for-major-mode 'magit-status-mode
;;     "gwb" 'magit-worktree-branch
;;     "gwm" 'magit-worktree-move
;;     "gwd" 'magit-worktree-delete
;;     "gws" 'magit-worktree-status
;;     "gwc" 'magit-worktree-checkout))

;; (add-hook 'magit-status-mode-hook #'my-magit-worktrees-keybindings)

;; ---------------------------------------
;; Helm Descbinds
;; Recent release of helm-descbinds package breaks which-key menu
;; Remove helm-discbinds-mode from helm mode hook to avoid activating
;; https://github.com/syl20bnr/spacemacs/issues/16276
(remove-hook 'helm-mode-hook 'helm-descbinds-mode)
;; ---------------------------------------
