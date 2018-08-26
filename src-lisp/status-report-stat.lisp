(defparameter *group-non-work* "non-work")
(defparameter *group-non-computer-related-work* "non-computer-related work")
(defparameter *group-project-management* "project-management")
(defparameter *group-computer-related-work* "computer-related-work")

(defmacro prompt-for-input (var prompt-message)
    `(progn
           (format t ,prompt-message)
	       (finish-output)
	       (setf ,var (read-line))))

(defmacro for-line (line in-stream &rest body)
  `(loop for ,line = (read-line ,in-stream nil 'eof)
     until (eq ,line 'eof)
     do
     	,@body))

(defparameter *status-report-file-name* nil)

(prompt-for-input *status-report-file-name* "Please enter the status-report file name: ")
(with-open-file (in *status-report-file-name*)
  (for-line line in
      (format t "line: ~A~%" line)))
