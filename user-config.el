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

(setq-default telega-server-libs-prefix "/usr/lib")

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
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
  ;; (face-remap-add-relative 'default 'treemacs-window-background-face)
  ;; (face-remap-add-relative 'fringe  'treemacs-window-background-face)
  ;; (face-remap-add-relative 'hl-line 'treemacs-hl-line-face)
  (solaire-global-mode +1))

;; ---------------------------------------
;; Searching
(evil-global-set-key 'normal
                     "/" (alist-get :buffer-search-tool config))
;;
;; ---------------------------------------

;; ---------------------------------------
;; Helm Descbinds
;; Recent release of helm-descbinds package breaks which-key menu
;; Remove helm-discbinds-mode from helm mode hook to avoid activating
;; https://github.com/syl20bnr/spacemacs/issues/16276
(remove-hook 'helm-mode-hook 'helm-descbinds-mode)
;; ---------------------------------------
