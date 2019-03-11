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

; the program starts here

; define constants
(defparameter *group-non-work* "non-work")
(defparameter *group-non-computer-related-work* "non-computer related work")
(defparameter *group-project-management* "project management")
(defparameter *group-computer-related-work* "computer-related work")

(with-open-file (in-stream (prompt-for-input "Please enter the status-report file name: "))
  (let (work-hours-table (make-hash-table))
    (for-line line in-stream
      (print-line line))))
