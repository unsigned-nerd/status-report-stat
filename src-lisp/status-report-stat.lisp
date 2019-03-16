(defmacro prompt-for-input (prompt-message &optional var)
   `(progn
     (format t ,prompt-message)
     (finish-output)
     ,(if (null var)
       `(read-line)
       `(setf ,var (read-line)))))

(defmacro for-line (line in-stream &rest body)
  `(loop for ,line = (read-line ,in-stream nil 'eof)
     until (eq ,line 'eof)
     do
     	,@body))

(defmacro print-line (line)
  `(format t "~A~%" ,line))

(defun parse-decimal (in-str)
  (with-input-from-string (in in-str)
    (read in)))

; load libraries
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
      (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :cl-ppcre)

; the program starts here

(let ((work-hours-ht (make-hash-table))

      ; the current work category key which is changed everytime we encounter a starting of the
      ; new work category
      (cur-work-cat-key)

      ; regex scanners
      (non-work-regex (cl-ppcre:create-scanner "^non-work"))
      (non-computer-related-work-regex (cl-ppcre:create-scanner "^non-computer related work"))
      (project-management-regex (cl-ppcre:create-scanner "^project management"))
      (computer-related-work-regex (cl-ppcre:create-scanner "^computer related work"))
      (time-estimation-regex (cl-ppcre:create-scanner
        "^[ ]*time estimation:[ ]*([0-9.]+)[ ]*hour.*")))

    ; initilize the hash table with the 4 keys to hold the accumulated total work hours of each work
    ; category
    (setf (gethash 'non-work work-hours-ht) 0)
    (setf (gethash 'non-computer-related-work work-hours-ht) 0)
    (setf (gethash 'project-management work-hours-ht) 0)
    (setf (gethash 'computer-related-work work-hours-ht) 0)

  (with-open-file (in-stream (prompt-for-input "Please enter the status-report file name: "))
    (for-line line in-stream
      (cond ((cl-ppcre:scan non-work-regex line)
              (setf cur-work-cat-key 'non-work))
            ((cl-ppcre:scan non-computer-related-work-regex line)
              (setf cur-work-cat-key 'non-computer-related-work))
            ((cl-ppcre:scan project-management-regex line)
              (setf cur-work-cat-key 'project-management))
            ((cl-ppcre:scan computer-related-work-regex line)
              (setf cur-work-cat-key 'computer-related-work))
            ((cl-ppcre:scan time-estimation-regex line)
               (cl-ppcre:register-groups-bind (work-hours) (time-estimation-regex line :sharedp t)
                  (incf (gethash cur-work-cat-key work-hours-ht) (parse-decimal work-hours))))))

    (print-line (gethash 'non-work work-hours-ht))
    (print-line (gethash 'non-computer-related-work work-hours-ht))
    (print-line (gethash 'project-management work-hours-ht))
    (print-line (gethash 'computer-related-work work-hours-ht))))
