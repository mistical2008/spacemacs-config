(defun etlk/config-utils-apply-config-files (config-files)
  "Apply configuration from the 'config-files' list"
  (dolist (file config-files)
    (let ((file-path (file-truename (concat dotspacemacs-directory file))))
      (when (file-readable-p file-path)
        (load file-path)))))

(provide 'etlk/config-utils)
