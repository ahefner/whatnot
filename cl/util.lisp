


(defun histogram (seq)
  (let ((histogram (make-hash-table :test 'equal)))
    (map nil (lambda (element) (incf (gethash element histogram 0))) seq)
    histogram))

(defun printhash (x &optional (*standard-output* *standard-output*))
  (maphash (lambda (key value) (format t "~&~A => ~A~%" key value)) x))

(defun hash-fold (hash init fn)
  (maphash (lambda (key value)
             (setf init (funcall fn key value init)))
           hash)
  init)

(defun hash-fold-values (hash init fn)
  (hash-fold hash
             init
             (lambda (key value accum)
               (declare (ignore key))
               (funcall fn value accum))))

(defun histogram-entropy (histogram)
  (let ((scale (/ 1.0 (hash-fold-values histogram 0 (lambda (N sum) (+ sum N))))))
    (* scale (hash-fold-values histogram 0 (lambda (N accum) (+ accum (* N (log (* scale n) 0.5))))))))

(defun read-forms (stream)
  (loop with eof = '#:eof
     as form = (read stream nil eof)
     until (eql form eof)
     collect form))

(defun read-file-forms (filename)
  (with-open-file (in filename)
    (read-forms in)))

(defun average-seq (seq)
  (if (zerop (length seq))
      0
      (/ (reduce #'+ seq) (float (length seq)))))

(defun sliding-entropy (window-size sequence)
  (loop with len = (length sequence)
        for i from 0 below (- len window-size)
        collect (histogram-entropy (histogram (subseq sequence i (+ i window-size))))))

;;; This definition isn't useful.. what would be?
#+NIL
(defun sliding-window-information-gain (window-size sequence)
  (map 'vector
       (lambda (x) (- (log window-size 2) x))
       (sliding-entropy window-size sequence)))

(defun extract-column (index seq)
  (map 'vector (lambda (row) (elt row index)) seq))

(defun diff-seq (sequence &optional (result-type 'vector))
  (map result-type #'- sequence (concatenate 'vector #(0) sequence)))

(defun diff-latched-seq (sequence &optional (result-type 'vector))
  (let ((last-value 0))
    (map result-type
         (lambda (x)
           (prog1 (and x (- x last-value))
             (setf last-value (or x last-value))))
         sequence)))
