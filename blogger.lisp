(in-package :blogger)

(defvar *author* nil)
(defvar *email* nil)
(defvar *passwd* nil)
(defvar *blog-id* nil)

(defvar *blogger* nil)

(load (merge-pathnames #p".blogger.lisp" (user-homedir-pathname)))

;; Drakma の設定
;; UTF-8
(setq *drakma-default-external-format* :utf-8)
;; application/atom+xml をバイナリではなくテキストとして扱う。
(pushnew (cons "application" "atom+xml") drakma:*text-content-types*
         :test #'equal)

;; s-xml
;; ネームスペースを使わない
(setf s-xml:*ignore-namespaces* t)

(defun escape-string (string)
  (with-output-to-string (out)
    (loop for c across string
          do (format out "~a" (case c
                                (#\& "&amp;")
                                (#\< "&lt;")
                                (#\> "&gt;")
                                (#\" "&quot;")
                                (t c))))))

(defclass blogger ()
  ((sid :initform nil :accessor sid)
   (lsid :initform nil :accessor lsid)
   (auth :initform nil :accessor auth)
   (blog-id :initform *blog-id* :accessor blog-id)
   (author :initform *author* :accessor author)
   (email :initform *email* :accessor email)
   (passwd :initform *passwd* :accessor passwd)
   (latest-entry :initform nil :accessor latest-entry)))

(defmethod login-parameters ((blogger blogger))
  `(("Email" . ,(email blogger))
    ("Passwd" . ,(passwd blogger))
    ("service" . "blogger")
    ("source" . "common-lisp")))


(defmethod login ((blogger blogger))
  (register-groups-bind (sid lsid auth)
      ((create-scanner "^SID=(.*)\\nLSID=(.*)\\nAuth=(.*)$" :multi-line-mode t)
       (http-request "https://www.google.com/accounts/ClientLogin"
                     :method :post
                     :parameters (login-parameters blogger)))
    (or auth (error "loign failed."))
    (setf (sid blogger) sid
          (lsid blogger) lsid
          (auth blogger) auth)))

(defmethod request ((blogger blogger) url &rest rest)
  (apply #'http-request
         url
         :additional-headers
         `(("Authorization" . ,(format nil "GoogleLogin auth=~a"
                                       (auth blogger))))
         rest))

(defmethod list-of-blogs ((blogger blogger))
  (request blogger "https://www.blogger.com/feeds/default/blogs"))

(defmethod retrive-posts ((blogger blogger))
  (request
   blogger
   (format nil "https://www.blogger.com/feeds/~a/posts/default"
           (blog-id blogger))))

(defmethod retrive-entry ((blogger blogger) entry-id)
  (let ((res (request
              blogger
              (format nil "https://www.blogger.com/feeds/~a/posts/default/~a"
                      (blog-id blogger) entry-id))))
    (print res)
    (setf (latest-entry blogger) (s-xml:parse-xml-string res))))



(defmethod send-entry ((blogger blogger) url method post-data)
  (print post-data)
  (let ((res (request blogger
                      url
                      :method method
                      :content-type "application/atom+xml"
                      :content-length
                      (length (flexi-streams:string-to-octets
                               post-data :external-format :utf-8))
                      :content post-data)))
    (print res)
    (setf (latest-entry blogger) (s-xml:parse-xml-string res))))

(defmethod prepare-entry ((blogger blogger) labels title contents)
  (with-output-to-string (result)
    (format result "<entry xmlns='http://www.w3.org/2005/Atom'>~%")
    (dolist (label labels) 
      (format result "<category scheme='http://www.blogger.com/atom/ns#' term='~a'/>~%" label))
    (format result "<title type='text'>~a</title>
<content type='xhtml'>~a</content>~%<author>~%<name>~a</name>~%<email>~a</email>
  </author>~%</entry>" title contents (author blogger) (email blogger))
    result
    )
  )

(defmethod post-entry ((blogger blogger) labels title contents)
  (let*
      ((url (format nil "https://www.blogger.com/feeds/~a/posts/default"
                    (blog-id blogger)))
       (post-data (prepare-entry blogger labels title contents)))
    (send-entry blogger url :post post-data)))

(defmethod edit-entry ((blogger blogger) labels title content)
  (replace-labels blogger labels)
  (replace-title blogger title)
  (replace-content blogger content)
  (let ((post-data (s-xml:print-xml-string (latest-entry blogger ))))
    (send-entry blogger
                ;; (print (edit-href blogger))
                (print (ppcre:regex-replace "^httpss" (edit-href blogger) "https"))
                :put
                post-data)))

(defmethod replace-xml ((blogger blogger) tag text)
  (setf (cdr (find tag (latest-entry blogger) :key #'find-key))
        (list text)))

(defmethod replace-labels ((blogger blogger) labels)
  (dolist (item (latest-entry blogger))
    (if (find :|category| (list item) :key #'find-key)
	(setf (latest-entry blogger) (delete item (latest-entry blogger)))))
  (dolist (label labels)
    (setf (cddddr (latest-entry blogger))
	  (cons (list (append 
		       '(:|category| :|scheme| "http://www.blogger.com/atom/ns#" :|term|)
		       (list label)))
		(cddddr (latest-entry blogger))))))

(defmethod replace-title ((blogger blogger) title)
  (replace-xml blogger :|title| title))

(defmethod replace-content ((blogger blogger) content)
  (replace-xml blogger :|content| content))

(defmethod edit-href ((blogger blogger))
  ;; Plato Wu,2009/02/24: replace https instead of http
  (let ((href (getf (cdar
		(find '(:|link| :|rel| "edit")
		      (latest-entry blogger)
		      :key #'(lambda (x)
			       (and (consp (car x))
				    (=  (length (car x)) 7)
				    (subseq (car x) 0 3)))
		      :test #'equal))
	       :|href|)))
    (regex-replace "http" href "https")
    ))

(defun find-key (x)
  (and (consp (car x)) (caar x)))

(defmethod delete-entry ((blogger blogger))
  (request blogger (edit-href blogger) :method :delete))

(defun get-additional-info (muse-file)
  ;; Plato Wu,2009/03/03: Modify to suppost label
  (let (title post-id labels)
    (with-open-file (in muse-file)
      (loop for l = (read-line in nil nil)
            while l
            do (progn
                 (register-groups-bind (ttl)
                     ("^#title\\s*(.+)" l)
                   (or title (setf title (escape-string ttl))))
                 (register-groups-bind (pstid)
                     ("^; post-id (.+)" l)
                   (setf post-id pstid))
		 (register-groups-bind (labelstring)
                     ("^; *[lL]abels[:：]{0,1} *(.+)" l)
                   (setf labels (split "[,，]\\s*" labelstring))))))
    (values title post-id labels)))

(defun html-file (muse-file)
  (let ((file (make-pathname :directory "/tmp"
                             :type "html"
                             :defaults muse-file)))
    (if (probe-file file)
      file
      (make-pathname :directory "/tmp"
                     :type (format nil "~a.html" (pathname-type muse-file))
                     :defaults muse-file))))

(defun need-space-char-p (char)
  (not
   (loop for i in '("CJK" "Hiragana" "Katakana"
                    "Halfwidth and Fullwidth Forms")
         with code-block = (cl-unicode:code-block char)
         thereis (search i code-block))))

(defun need-space-p (current next)
  (cond ((null next)
         nil)
        ((string= next "")
         nil)
        ((scan "^<p>" next)
         nil)
        ((scan "</p>$" current)
         nil)
        ((string= current "")
         t)
        ((need-space-char-p (char current (1- (length current))))
         t)
        ((need-space-char-p (char next 0))
         t)))

(defun get-content-from-file (file)
  "Remove #\Newline.
We need a #\Newline in pre tag.
We need a space between lines in English.
We do not need any space between lines in Japanese."
  (with-output-to-string (out)
    (with-open-file (in file)
      (loop with pre-p = nil
            for current = (read-line in nil) then next
            for next = (read-line in nil)
            while current
            do (write-string current out)
            do (cond ((scan "<pre[^>]*>.+$" current)
                      (setf pre-p t)
                      (terpri out))
                     ((eql 0 (search "<pre" current))
                      (setf pre-p t))
                     ((search "</pre>" current)
                      (setf pre-p nil))
                     (pre-p
                      (terpri out))
                     ((need-space-p current next)
                      (write-string " " out)))))))

(defun add-post-id-to-file (muse-file)
  (register-groups-bind (post-id) (".*/(.*)" (edit-href *blogger*))
    (let ((content (with-output-to-string (out)
                     (with-open-file (in muse-file)
                       (loop for c = (read-char in nil nil)
                             while c
                             do (write-char c out))))))
      (with-open-file (out muse-file :direction :output :if-exists :supersede)
        (write-string content out)
        (format out "~&; post-id ~a~%" post-id)))))

;; api
(defun post (muse-file)
  (setf *blogger* (make-instance 'blogger))
  (login *blogger*)
  ;; Plato Wu,2009/03/12: need use a elegant way to refactory html-file function
  (let ((content (get-content-from-file (html-file muse-file))))
    (multiple-value-bind (title post-id labels) (get-additional-info muse-file)
      (if post-id
        ;; 修正
        (progn
          (retrive-entry *blogger* post-id)
          (edit-entry *blogger* labels title content))
        ;; 新規
        (progn
          (post-entry *blogger* labels title content)
          (add-post-id-to-file muse-file)))))
  *blogger*)
