(in-package :blogger)

(defvar *blog-id*)
(defvar *client-id*)
(defvar *client-secret*)
(defvar *refresh-token*)

(defvar *blogger* nil)

(load (merge-pathnames #p".blogger.lisp" (user-homedir-pathname)))


;; Drakma の設定
;; UTF-8
(setq *drakma-default-external-format* :utf-8)
;; バイナリではなくテキストとして扱う。
(pushnew (cons "application" "json") drakma:*text-content-types*
         :test #'equal)

#+nil
(defun how-to-get-refresh-token ()
  ;; ブラザでこの URL にアクセスしアクセスコードを取得する
  (quri:make-uri :scheme "https"
                 :host "accounts.google.com"
                 :path "/o/oauth2/v2/auth"
                 :query (quri:url-encode-params
                         `(("client_id" . ,*client-id*)
                           ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                           ("scope" . "https://www.googleapis.com/auth/blogger")
                           ("response_type" . "code")
                           ("state" . "")
                           ("code_challenge_method" . "plain")
                           ("code_challenge" . ,(code-challenge)))))

  ;; 取得したアクセスコードをセットして次を実行すれば refresh_token を取得できる
  (let ((code "取得したアクセスコード"))
   (http-request "https://www.googleapis.com/oauth2/v4/token"
                 :method :post
                 :parameters `(("code" . ,code)
                               ("client_id" . ,*client-id*)
                               ("client_secret" . ,*client-secret*)
                               ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                               ("grant_type" . "authorization_code")
                               ("code_verifier" . ,(code-challenge))))))

(defun refresh-access-token ()
  (let* ((response (http-request "https://www.googleapis.com/oauth2/v4/token"
                                :method :post
                                :parameters `(("refresh_token" . ,*refresh-token*)
                                              ("client_id" . ,*client-id*)
                                              ("client_secret" . ,*client-secret*)
                                              ("grant_type" . "refresh_token"))))
         (json (jsonq:read-json-from-string response)))
    (jsonq:q json :access_token)))

(defclass blogger ()
  ((blog-id :initform *blog-id* :accessor blog-id)
   (latest-entry :initform nil :accessor latest-entry)
   (access-token :accessor .access-token)))

(defun code-challenge ()
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(defmethod login ((blogger blogger))
  (setf (.access-token blogger) (refresh-access-token)))

(defmethod request ((blogger blogger) url &rest rest)
  (apply #'http-request
         url
         :additional-headers
         `(("Authorization" . ,(format nil "Bearer ~a"
                                       (.access-token blogger))))
         rest))

(defmethod retrive-entry ((blogger blogger) entry-id)
  (let ((res (request
              blogger
              (format nil "https://www.googleapis.com/blogger/v3/blogs/~a/posts/~a"
                      (blog-id blogger) entry-id))))
    (print res)
    (setf (latest-entry blogger) (jsonq:read-json-from-string res))))

(defmethod send-entry ((blogger blogger) url method post-data)
  (let ((res (request blogger
                      url
                      :method method
                      :content-type "application/json"
                      :content-length
                      (length (flexi-streams:string-to-octets
                               post-data :external-format :utf-8))
                      :content post-data)))
    (print res)
    (setf (latest-entry blogger) (jsonq:read-json-from-string res))))

(defmethod prepare-entry ((blogger blogger) labels title content)
  (princ-to-string
   (jsonq:obj
    :kind "blogger#post"
    :blog (jsonq:obj :id *blog-id*)
    title
    content)))

(defmethod post-entry ((blogger blogger) labels title contents)
  (let* ((url (format nil "https://www.googleapis.com/blogger/v3/blogs/~a/posts"
                      (blog-id blogger)))
         (post-data (prepare-entry blogger labels title contents)))
    (send-entry blogger url :post post-data)))

(defmethod edit-entry ((blogger blogger) labels title content)
  (replace-labels blogger labels)
  (replace-title blogger title)
  (replace-content blogger content)
  (let ((post-data (princ-to-string (latest-entry blogger))))
    (send-entry blogger
                (format nil "https://www.googleapis.com/blogger/v3/blogs/~a/posts/~a"
                        (blog-id blogger) (jsonq:q (latest-entry blogger) :id))
                :put
                post-data)))

(defmethod replace-labels ((blogger blogger) labels)
  (setf (latest-entry blogger)
        (jsonq:obj * (latest-entry blogger) labels)))

(defmethod replace-title ((blogger blogger) title)
  (setf (latest-entry blogger)
        (jsonq:obj * (latest-entry blogger) title)))

(defmethod replace-content ((blogger blogger) content)
    (setf (latest-entry blogger)
        (jsonq:obj * (latest-entry blogger) content)))

(defun get-additional-info (original-file)
  ;; Plato Wu,2009/03/03: Modify to suppost label
  (let (title post-id labels)
    (with-open-file (in original-file)
      (loop for l = (read-line in nil nil)
            while l
            do (progn
                 (register-groups-bind (ttl)
                     ("^#\\s*(.+)" l)
                   (or title (setf title ttl)))
                 (register-groups-bind (pstid)
                     ("^<!-- post-id ([^ ]+) -->$" l)
                   (setf post-id pstid))
		 (register-groups-bind (labelstring)
                     ("^<!-- *[lL]abels[:：]{0,1} *(.+) *-->$" l)
                   (setf labels (split "[,，]\\s*" labelstring))))))
    (values title post-id labels)))

(defun html-file (original-file)
  (let ((file (make-pathname :type "html"
                             :defaults original-file)))
    (if (probe-file file)
      file
      (make-pathname :directory "/tmp"
                     :type (format nil "~a.html" (pathname-type original-file))
                     :defaults original-file))))

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
    (with-open-stream (in (make-string-input-stream
                           (lquery:$1 (initialize file) "article" (serialize))))
      (loop with pre-p = nil
            for current = (read-line in nil) then next
            for next = (read-line in nil)
            while (and current (string/= current "<script>"))
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

(defun add-post-id-to-file (original-file)
  (let ((content (with-output-to-string (out)
                   (with-open-file (in original-file)
                     (loop for c = (read-char in nil nil)
                           while c
                           do (write-char c out))))))
    (with-open-file (out original-file :direction :output :if-exists :supersede)
      (write-string content out)
      (format out "~&<!-- post-id ~a -->~%" (jsonq:q (latest-entry *blogger*) :id)))))

;; api
(defun post (original-file)
  (setf *blogger* (make-instance 'blogger))
  (login *blogger*)
  ;; Plato Wu,2009/03/12: need use a elegant way to refactory html-file function
  (let* ((html-file (html-file original-file))
         (content (get-content-from-file html-file)))
    (multiple-value-bind (title post-id labels) (get-additional-info original-file)
      (if post-id
          ;; 修正
          (progn
            (retrive-entry *blogger* post-id)
            (edit-entry *blogger* labels title content))
          ;; 新規
          (progn
            (post-entry *blogger* labels title content)
            (add-post-id-to-file original-file))))
    (delete-file html-file))
  *blogger*)
