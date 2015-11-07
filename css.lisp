(in-package :info.read-eval-print.css)

(defvar *buffer* nil)

(defmacro with-css-buffer ((&optional buffer) &body body)
  `(let ((*buffer* (or ,buffer (make-array 256 :adjustable t :fill-pointer 0))))
     ,@body
     *buffer*))

(defun emit (format &rest args)
  (vector-push-extend
   (sb-ext:string-to-octets (apply #'format nil format args))
   *buffer*))

(defun s (x)
  (typecase x
    (symbol
     (let ((name (symbol-name x)))
       (if (some #'lower-case-p name)
           name
           (string-downcase name))))
    (t (princ-to-string x))))

(defun split-properties (form &optional acc)
  (let ((pos (position-if #'keywordp form :start 1)))
    (if pos
        (split-properties (subseq form pos) (cons (subseq form 0 pos) acc))
        (nreverse (cons form acc)))))

(defun write-selector (selector)
  (emit "~&~{~a~^ ~}" (mapcar #'s selector)))

(defun write-property (property)
  (emit "~a:" (s (car property)))
  (emit "~{~a~^ ~};" (mapcar #'s (cdr property))))

(defun flatten (form &optional parent-selector)
  (if (null form)
      nil
      (let* ((pos (position-if (lambda (x) (or (keywordp x) (consp x))) form :start 1))
             (selector (subseq form 0 pos))
             (rest (subseq form pos))
             (properties (remove-if #'consp rest))
             (children (loop for child in (remove-if-not  #'consp rest)
                             nconc (flatten child (append parent-selector selector)))))
        (if properties
            (cons `(,(append parent-selector selector) ,properties)
                  children)
            children))))

(defun css (forms)
  (mapc (lambda (form)
          (loop for (selector properties) in (flatten form) do
            (write-selector selector)
            (emit "{")
            (mapc #'write-property (split-properties properties))
            (emit "}")))
        forms))
