;; ---------------------------------------
;; Org-mode configuration
;; ---------------------------------------

(defconst task-points-enum '(1 3 5 8 13 21 34))

(defconst path-keys '(
                      (:drafts  . "~/03_Drafts")
                      (:org-dir . "org")
                      (:inbox   . "00_inbox.org")
                      (:todo    . "01_todo.org")
                      (:notes   . "02_notes.org")))

(defconst etlk/org-path
      '(:drafts :org-dir))

(defun create-file-path (&rest constant-names)
  "Create a file path using the given constant names."
  (let* ((paths (mapcar (lambda (name)
                          (cdr (assoc name path-keys)))
                        constant-names))
         (full-path (mapconcat 'identity paths "/")))
    (expand-file-name full-path)))

(defun etlk/get-org-dir ()
  (apply #'create-file-path etlk/org-path))

(defun etlk/create-org-path (paths)
  (apply #'create-file-path (append etlk/org-path paths)))

;; ------------------------------------------
;; Path getters
;; ------------------------------------------
(defun etlk/get-todos-path ()
  (etlk/create-org-path '(:todo)))

(defun etlk/get-todos-inbox-path ()
  (etlk/create-org-path '(:inbox)))

(defun etlk/get-notes-path ()
  (etlk/create-org-path '(:notes)))

(defun etlk/get-journal-path ()
  (etlk/create-org-path '(:journal)))

;; ------------------------------------------
;; Fabrics
;; ------------------------------------------
(defun etlk/create-task-points-string ()
  (mapconcat 'number-to-string task-points-enum " "))

(defun etlk/create-task-points-prompts ()
  (mapconcat 'number-to-string task-points-enum "|"))

(defconst points-all (etlk/create-task-points-string))
;; ---------------------------------------
;; Configuration
;; ---------------------------------------
(setq
 org-directory (etlk/get-org-dir)
 org-agenda-files (list "01_todo.org"))

(with-eval-after-load 'org
  (setq
   org-tag-alist '(("@career"        . ?c)
                   ("@finance"       . ?m)
                   ("@health"        . ?h)
                   ("@sport"         . ?s)
                   ("@entertainment" . ?e)
                   ("@family"        . ?f)
                   ("@work"          . ?w)
                   ("@personal"      . ?p))

   org-roam-directory (etlk/get-org-dir)

   org-global-properties (list (cons "points_ALL" points-all))

   org-capture-templates
   '(("t" "Today task"
      entry (file+headline etlk/get-todos-path "Today")
      "** TODO [#B] %? %^G\n:Created: %T\n:PROPERTIES:\n:points: %^{points|%(etlk/create-task-points-prompts)}\n:END:\n"
      :empty-lines 1)

     ("w" "Current week task"
      entry (file+headline etlk/get-todos-path "Current week")
      "** TODO [#C] %? %^G\n:Created: %T\n:PROPERTIES:\n:points: %^{points|%(etlk/create-task-points-prompts)}\n:END:\n"
      :empty-lines 1)

     ("l" "Task for the later"
      entry (file+headline etlk/get-todos-path "Later")
      "** TODO [#D] %? %^G\n:Created: %T\n:PROPERTIES:\n:points: %^{points|%(etlk/create-task-points-prompts)}\n:END:\n"
      :empty-lines 1)

     ("r" "Reccurent task or habit"
      entry (file+headline etlk/get-todos-path "Recur")
      "** TODO [#C] %? %^G\n:Created: %T\nSCHEDULED: %t\n:PROPERTIES:\n:style:\thabit\n:points: %^{points|%(etlk/create-task-points-prompts)}\n:END:\n"
      :empty-lines 1)

     ("i" "Inbox task"
      entry (file+headline etlk/get-todos-path "Inbox")
            "** TODO %?\n:Created: %T\n"
            :empty-lines 1))

   ;; Define the location of the file to hold tasks
   org-default-notes-file (etlk/get-notes-path)
   org-journal-dir        (etlk/get-journal-path)

   ;; Define a kanban style set of stages for todo tasks
   org-todo-keywords '((sequence "TODO(t!)" "DOING(p)" "BLOCKED(b)" "REVIEW(r)" "|" "DONE(d!)" "ARCHIVED(a)" "CANCELLED(c@/!)"))

   ;; Progress Log - add CLOSED: property & current date-time when TODO item enters DONE
   org-log-done 'time))

(org-roam-db-autosync-mode)

(with-eval-after-load 'org-agenda
   ;; (require 'org-projectile)
   ;; (push (org-projectile:todo-files) org-agenda-files))

   ;; ---------------------------------------
   ;; Org-super-agenda
   ;; ---------------------------------------
   (setq org-agenda-prefix-format "  %t  %s")
   (org-super-agenda-mode)

   ;; Setting colours (faces) of task states
   ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el#L376
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   ;; Using `with-eval-after-load' as a hook to call this setting when org-mode is run
   org-todo-keyword-faces
   '(("TODO" . "Gold")
     ("DOING" . "DeepSkyBlue")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue")
     ("CANCELLED" .  "PaleVioletRed")))

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



(setq org-agenda-custom-commands
 '(("u" "Super view"
    ((agenda "" (
                 (org-agenda-span 3)
                 (org-agenda-prefix-format "  %t  %s")
                 (org-super-agenda-groups
                  '((:name none
                           :time-grid t
                           :date today
                           :order 1)))))
     (todo "" (
               (org-agenda-overriding-header "TODOs")
               (org-agenda-prefix-format "  %t  %s")
               (org-super-agenda-groups
                '(
                  ;; (:auto-group t)
                  (:auto-outline-path t :auto-parent t)
                  (:name "Crytical"
                         :priority "A"
                         :order 0)
                  ;; (:name "Today (Important)"
                  ;;        ;; :heading-regexp "Today"
                  ;;        :and (
                  ;;              :heading-regexp ("/Today/")
                  ;;              :priority "A"
                  ;;              :not (:todo "DONE"))
                  ;;        :order 2)
                  ;; (:name "Today"
                  ;;        ;; :auto-todo t
                  ;;        ;; :auto-parent t
                  ;;        :and (
                  ;;              :heading-regexp ("Today")
                  ;;              :not (:todo "DONE" :priority "A"))
                  ;;        :order 3)
                  (:name "Habits"
                         :habit t
                         :order 4)
                  (:name "Done"
                         :todo "DONE"
                         :regexp "State \"DONE\""
                         :log t
                         :order 5)))))))))

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
