(defvar muse-blogger-markup-strings '()
  "Strings used for marking up text as XHMTL for Blogger.
Example:
((section . \"<h4>\")
 (section-end . \"</h4>\")
 (subsection . \"<h5>\")
 (subsection-end . \"</h5>\")
 (subsubsection . \"<h6>\")
 (subsubsection-end . \"</h6>\")
 (section-other . \"<div>\")
 (section-other-end . \"</div>\")
 (begin-example . \"<pre class='src'>\"))")

(muse-derive-style
 "blogger" "xhtml1.1"
 :header  ""
 :footer  ""
 :strings 'muse-blogger-markup-strings)

(defun blogger-post ()
  (interactive)
  (save-buffer)
  (muse-publish-this-file (muse-style "blogger") "/tmp" t)
  (slime-repl-send-string
   (format "(progn (require :blogger) (funcall (read-from-string \"blogger:post\") #p\"%s\"))"
           (buffer-file-name))))

(defun muse-html-markup-footnote ()
  (cond
   ((get-text-property (match-beginning 0) 'muse-link)
    nil)
   ((= (muse-line-beginning-position) (match-beginning 0))
    (prog1
        (let ((text (match-string 1)))
          (muse-insert-markup
           (concat "<p class=\"footnote\">"
                   "<a class=\"footnum\" name=\"" (muse-publishing-directive "title") "fn." text
;                   "\" href=\"#fnr." text "\">"
		   "\" href=\"#" (muse-publishing-directive "title") "fnr." text "\">"
                   text ".</a>")))
      (save-excursion
        (save-match-data
          (let* ((beg (goto-char (match-end 0)))
                 (end (and (search-forward "\n\n" nil t)
                           (prog1
                               (copy-marker (match-beginning 0))
                             (goto-char beg)))))
            (while (re-search-forward (concat "^["
                                              muse-regexp-blank
                                              "]+\\([^\n]\\)")
                                      end t)
              (replace-match "\\1" t)))))
      (replace-match "")))
   (t (let ((text (match-string 1)))
        (muse-insert-markup
         (concat "<sup><a class=\"footref\" name=\"" (muse-publishing-directive "title") "fnr." text
;                 "\" href=\"#fn." text "\">"
                 "\" href=\"#" (muse-publishing-directive "title") "fn." text "\">"
                 text "</a></sup>")))
      (replace-match ""))))