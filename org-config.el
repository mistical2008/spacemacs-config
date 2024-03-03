;; ---------------------------------------
;; Org-mode configuration
;; ---------------------------------------

;; ---------------------------------------
;; Notes and Tasks

(with-eval-after-load 'org
  (setq
   ;; Define the location of the file to hold tasks
   org-default-notes-file "~/03_Drafts/01_tasks/todo.org"

   ;; Define a kanban style set of stages for todo tasks
   org-todo-keywords '((sequence "TODO(t)" "DOING(p)" "BLOCKED(b)" "REVIEW(r)" "|" "DONE(d)" "ARCHIVED(a)"))

   ;; Progress Log - add CLOSED: property & current date-time when TODO item enters DONE
   org-log-done 'time))

(with-eval-after-load 'org-agenda
  ;; (require 'org-projectile)
  ;; (push (org-projectile:todo-files) org-agenda-files))

   ;; Setting colours (faces) of task states
   ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el#L376
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   ;; Using `with-eval-after-load' as a hook to call this setting when org-mode is run
   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("DOING" . "DarkOrchid")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue")))

;; ;; Set TODO keyword faces if over-ridden by theme.
(defun practicalli/set-todo-keyword-faces ()
  (interactive)
  (setq hl-todo-keyword-faces
        '(("TODO" . "SlateGray")
          ("DOING" . "DarkOrchid")
          ("BLOCKED" . "Firebrick")
          ("REVIEW" . "Teal")
          ("DONE" . "ForestGreen")
          ("ARCHIVED" .  "SlateBlue"))))
;; ---------------------------------------


;; ---------------------------------------
;; customize org-mode's checkboxes with unicode symbols
(add-hook
 'org-mode-hook
 (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐") prettify-symbols-alist)
   (push '("[X]" . "☑" ) prettify-symbols-alist)
   (push '("[-]" . "❍" ) prettify-symbols-alist)
   (prettify-symbols-mode)))
;; ---------------------------------------
