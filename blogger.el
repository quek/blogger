(defun blogger-post ()
  (interactive)
  (save-buffer)
  (markdown-export)
  (slime-repl-send-string
   (format "(progn (quicklisp:quickload :blogger) (funcall (read-from-string \"blogger:post\") #p\"%s\"))"
           (buffer-file-name))))
