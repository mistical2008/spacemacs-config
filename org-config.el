;; ---------------------------------------
;; Org-mode configuration
;; ---------------------------------------

(setq constants '(
                  (:drafts  . "~/03_Drafts")
                  (:org-dir . "org")
                  (:inbox   . "00_inbox.org")
                  (:todo    . "01_todo.org")
                  (:notes   . "02_notes.org")))

(defun create-file-path (&rest constant-names)
  "Create a file path using the given constant names."
  (let* ((paths (mapcar (lambda (name)
                          (cdr (assoc name constants)))
                        constant-names))
         (full-path (mapconcat 'identity paths "/")))
    (expand-file-name full-path)))

(create-file-path :drafts :org-dir)

(setq etlk/org-path
  '(:drafts :org-dir))


(defun etlk/get-org-dir ()
  (apply #'create-file-path etlk/org-path))

;; ---------------------------------------
;; Notes and Tasks

(with-eval-after-load 'org
  (setq
   org-tag-alist '(
                   ("@career"        . ?c)
                   ("@finance"       . ?m)
                   ("@health"        . ?h)
                   ("@sport"         . ?s)
                   ("@entertainment" . ?e)
                   ("@family"        . ?f)
                   ("@work"          . ?w)
                   ("@personal"      . ?p))

   org-roam-directory (etlk/get-org-dir)

   org-capture-templates
   '(("t" "General task"
      entry (file+headline (lambda () (concat (etlk/get-org-dir) "/01_todo.org" )) "General tasks")
            "* TODO [#B] %?\n:Created: %T\n"
            :empty-lines 0)
     ("i" "Inbox note"
      entry (file+headline (lambda () (concat (etlk/get-org-dir) "/00_inbox.org")) "General tasks")
            "* TODO %?\n:Created: %T\n"
            :empty-lines 0))
   ;; Define the location of the file to hold tasks
   org-default-notes-file (concat (etlk/get-org-dir) "/02_notes.org")
   org-journal-dir (concat (etlk/get-org-dir) "/03_journal")

   ;; Define a kanban style set of stages for todo tasks
   org-todo-keywords '((sequence "TODO(t)" "DOING(p)" "BLOCKED(b)" "REVIEW(r)" "|" "DONE(d)" "ARCHIVED(a)"))

   ;; Progress Log - add CLOSED: property & current date-time when TODO item enters DONE
   org-log-done 'time))

(org-roam-db-autosync-mode)

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
