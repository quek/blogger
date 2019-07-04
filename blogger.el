(defun blogger-post ()
  (interactive)
  (save-buffer)
  (auto-revert-mode 1)
  (markdown-export)
  (slime-repl-send-string
   (format "(progn (quicklisp:quickload :blogger) (funcall (read-from-string \"blogger:post\") #p\"%s\"))"
           (buffer-file-name))))
