(in-package :info.read-eval-print.css.test)

(defun css* (forms)
  (with-output-to-string (out)
    (loop for i across (with-css-buffer ()
                         (css forms))
          do (write-string (sb-ext:octets-to-string i) out))))


(deftest test-basic ()
  (is
   (string= "body{color:#000000;}"
            (css* `((body :color \#000000)))))
  (is
   (string= "h1{margin:0 2px 0 4px;color:red;}h2{text-align:center;}a:hover{font-size:12px;}"
            (css*
              `((h1 :margin 0 2px 0 4px :color red)
                (h2 :text-align center)
                (a\:hover :font-size 12px))))))

(run-package-tests :interactive t)
