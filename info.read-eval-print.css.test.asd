(asdf:defsystem :info.read-eval-print.css.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "test"))
  :depends-on (:info.read-eval-print.css
               :fiasco))
