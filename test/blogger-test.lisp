(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :blogger)
  (asdf:oos 'asdf:load-op :stefil)
  (use-package :stefil))

(defsuite blogger-test)

(in-suite blogger-test)

(deftest test-get-content-from-file ()
  (labels ((path (&optional (name "a.txt"))
             (merge-pathnames name *load-pathname*))
           (f (test-data expected)
             (with-open-file (out (path) :direction :output
                                  :if-exists :supersede)
               (write-string test-data out))
             (is (string= expected (blogger::get-content-from-file (path))))))
    (f
     "<p>This is
a test.</p>

<p>これは
テストです。</p>



<p>This is
テスト。</p>


<p>これは
test.</p>

<p><em>emphasis</em>
<strong>strong emphasis</strong>
<strong><em>very strong emphasis</em></strong>
<span style=\"text-decoration: underline;\">underlined</span>
<code>verbatim and monospace</code></p>

<pre class=\"src\">
(<span style=\"color: #00ffff;\">defun</span> <span style=\"color: #87cefa;\">foo</span> ()
  'foo)
あ
い
</pre>

<p>おしまい</p>
"
     "<p>This is a test.</p><p>これはテストです。</p><p>This is テスト。</p><p>これは test.</p><p><em>emphasis</em> <strong>strong emphasis</strong> <strong><em>very strong emphasis</em></strong> <span style=\"text-decoration: underline;\">underlined</span> <code>verbatim and monospace</code></p> <pre class=\"src\">(<span style=\"color: #00ffff;\">defun</span> <span style=\"color: #87cefa;\">foo</span> ()
  'foo)
あ
い
</pre><p>おしまい</p>")

    (f "<pre>
<span>aaa</span></pre>

<p>bbb</p>
"
   "<pre><span>aaa</span></pre><p>bbb</p>")))

(blogger-test)
