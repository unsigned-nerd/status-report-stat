; {{{ quicklisp initial set up
; load libraries
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
      (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
; }}} quicklisp initial set up

; {{{ my utils
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
; }}} my utils

; the program starts here

; There are 4 categories of work which are non-work, non-computer related work, project management
; and computer-related work.  We create a work-hours-ht hashtable to store the sum of the work hours
; of each work category by using the work cateogry as the key.  We loop through the work log input
; file line by line.  When we match (with regular expression) the starting point of each work
; category, we use a variable, cur-work-cat-key, to remember what the current work category is.
; When we match the "time estimation" line, we add the work hours into that work-hours-ht hashtable
; using the value of cur-work-cat-key as the key.

(require :cl-ppcre)
(import '(cl-ppcre:create-scanner cl-ppcre:scan cl-ppcre:register-groups-bind))

(let ((work-hours-ht (make-hash-table))
      (cur-work-cat-key)

      ; regex scanners
      (non-work-regex (create-scanner "^non-work"))
      (non-computer-related-work-regex (create-scanner "^non-computer related work"))
      (project-management-regex (create-scanner "^project management"))
      (computer-related-work-regex (create-scanner "^computer related work"))
      (time-estimation-regex (create-scanner "^[ ]*time estimation:[ ]*([0-9.]+)[ ]*hour.*")))

    ; initilize the hash table with the 4 keys to hold the accumulated total work hours of each work
    ; category
    (setf (gethash 'non-work work-hours-ht) 0)
    (setf (gethash 'non-computer-related-work work-hours-ht) 0)
    (setf (gethash 'project-management work-hours-ht) 0)
    (setf (gethash 'computer-related-work work-hours-ht) 0)

  (with-open-file (in-stream (prompt-for-input "Please enter the status-report file name: "))
    (for-line line in-stream
      (cond ((scan non-work-regex line)
              (setf cur-work-cat-key 'non-work))
            ((scan non-computer-related-work-regex line)
              (setf cur-work-cat-key 'non-computer-related-work))
            ((scan project-management-regex line)
              (setf cur-work-cat-key 'project-management))
            ((scan computer-related-work-regex line)
              (setf cur-work-cat-key 'computer-related-work))
            ((scan time-estimation-regex line)
               (register-groups-bind (work-hours) (time-estimation-regex line :sharedp t)
                  (incf (gethash cur-work-cat-key work-hours-ht) (parse-decimal work-hours)))))))

  (format t "You have spent your time on:~%~
             ~Cnon-work for ~$ hours~%~
             ~Cnon-computer related work for ~$ hours~%~
             ~Cproject management for ~$ hours~%~
             ~Ccomputer related work for ~$ hours~%"

    #\tab (gethash 'non-work work-hours-ht)
    #\tab (gethash 'non-computer-related-work work-hours-ht)
    #\tab (gethash 'project-management work-hours-ht)
    #\tab (gethash 'computer-related-work work-hours-ht)))
