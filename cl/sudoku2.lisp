
;; "Impossible" puzzle
(defparameter *puzzle1*
  '((2 1 2)
    (8 1 3)
    (9 1 8)
    (4 2 9)
    (2 3 3)
    (3 3 4)
    (6 3 1)
    (7 3 9)
    (5 4 4)
    (7 4 7)
    (9 4 3)
    (3 5 1)
    (4 5 5)
    (6 5 8)
    (7 5 6)
    (1 6 4)
    (3 6 7)
    (5 6 3)
    (3 7 2)
    (4 7 8)
    (7 7 3)
    (8 7 6)
    (6 8 5)
    (1 9 6)
    (2 9 8)
    (8 9 5)))

(defun stupid-puzzle ()
  (loop
     for base in '(0 3 6 4 7 1 5 8 2)
     for row upfrom 1
     nconcing (loop
		 for i from base
		 for col from 1 upto 9
		 collect (list col row (1+ (mod i 9))))))

(defun puzzle-array (markings)
  (loop with array = (make-array '(9 9) :initial-element 0)
     for (col row number) in markings
     do
       (assert (zerop (shiftf (aref array (1- row) (1- col)) number)))
     finally (return array)))

(defun array->puzzle (array)
  (loop
     for col from 0 below 9
     nconcing
       (loop
	  for row from 0 below 9
	  nconcing
	    (if (zerop (aref array row col))
		nil
		(list (list (1+ col) (1+ row) (aref array row col)))))))

(defun clear-screen ()
  (write-char (code-char 27))
  (write-string "[2J")
  (write-char (code-char 27))
  (write-string "[H"))

(defun puzzle-string (markings)
  
  (loop with string = (copy-seq
"   |   |   
   |   |   
   |   |   
-----------
   |   |   
   |   |   
   |   |   
-----------
   |   |   
   |   |   
   |   |   
")
     for (col row num) in markings
     as c = (+ (1- col) (floor (1- col) 3))
     as r = (+ (1- row) (floor (1- row) 3))
     as index = (+ c (* r 12))
     do (setf (elt string index) (code-char (+ (char-code #\0) num)))
     finally (return string)))
    

;; Easy puzzle
(defparameter *puzzle2-array*
  #2A((8 7 6 9 0 0 0 0 0)
      (0 1 0 0 0 6 0 0 0)
      (0 4 0 3 0 5 8 0 0)
      (4 0 0 0 0 0 2 1 0)
      (0 9 0 5 0 0 0 0 0)
      (0 5 0 0 4 0 3 0 6)
      (0 2 9 0 0 0 0 0 8)
      (0 0 4 6 9 0 1 7 3)
      (0 0 0 0 0 1 0 0 4)))

;; hard puzzle
(defparameter *puzzle3-array*
  #2A ((0 3 0 2 0 0 0 0 6)
       (0 0 0 0 0 9 0 0 4)
       (7 6 0 0 0 0 0 0 0)
       (0 0 0 0 5 0 7 0 0)
       (0 0 0 0 0 1 8 6 0)
       (0 5 0 4 8 0 0 9 0)
       (8 0 0 0 0 0 0 0 0)
       (0 0 0 0 7 6 0 0 0)
       (0 7 5 0 0 8 1 0 0)))

(defparameter *puzzle2* (array->puzzle *puzzle2-array*))
(defparameter *puzzle3* (array->puzzle *puzzle3-array*))


(defvar *solve-counter* 0)
(defvar *backtrack-counter* 0)

(defun block-index (col row)
  (assert (<= 1 col 9))
  (assert (<= 1 row 9))
  (+ (* 3 (floor (1- row) 3)) (floor (1- col) 3)))

(defun initial-constraints () (loop repeat 9 collecting #x1FF))

(Defun already-marked (mask col row)
  (not (zerop (ldb (byte 1 (+ (* row 10) col)) mask))))

(defun markbit (mask col row)
  (dpb 1 (byte 1 (+ (* row 10) col)) mask))

(defun pluck (sets index value)
  (if (zerop index)
      (cons (logxor (first sets) (ash 1 (1- value)))
	    (rest sets))
      (cons (first sets)
	    (pluck (rest sets) (1- index) value))))

(defun mark* (columns rows blocks map col row number)
  (assert (not (already-marked map col row)))
  (values
   (pluck columns (1- col) number)
   (pluck rows (1- row) number)
   (pluck blocks (block-index col row) number)
   (markbit map col row)))

(defun mark (sets point)
  (multiple-value-list
   (multiple-value-call #'mark* (values-list sets) (values-list point))))

(defun apply-moves (moves)
  (reduce #'mark
	  moves
	  :initial-value (list (initial-constraints)
			       (initial-constraints)
			       (initial-constraints)
			       0)))

(defun sorted-sets (sets)
  (sort
   (loop
      for index upfrom 1
      for set in sets
      collect (list index set))
   #'<
   :key (lambda (pair) (logcount (second pair)))))

(defun sorted-choices (sets)
  (destructuring-bind (colsets rowsets blocksets marked) sets
    (sort
     (loop
	for col from 1 upto 9
	for colset in colsets
	nconcing
	  (loop
	     for row from 1 upto 9
	     for rowset in rowsets
	     as blockset = (elt blocksets (block-index col row))
	     as possible = (logand colset rowset blockset)
	     as count = (logcount possible)
	     nconcing
	       (cond
		 ((already-marked marked col row)
		  nil)
		 (t
		  (list (list col row possible))))))
     #'<
     :key (lambda (x) (logcount (third x))))))



(defparameter *graphical-solver* nil)

(defvar *depth* 0)
(defvar *depth-counts* (make-array 82 :initial-element 0))

(defun solve (sets prev-moves &aux (*depth* (1+ *depth*)))
  ;;(print (puzzle-array prev-moves))
  (incf *solve-counter*)
  (incf (aref *depth-counts* *depth*))
  
  (when (and (zerop (mod *solve-counter* 50000)) (not *graphical-solver*))
    (format t "~&Searching.. called SOLVE ~:D times, ~:D squares marked.~%" *solve-counter* *backtrack-counter*))

  (when *graphical-solver*
    (clear-screen)
    (write-string (puzzle-string prev-moves))
    (format t "~%~%Searching.. called SOLVE ~:D times, backtracked ~:D, depth ~:D~%"
	    *solve-counter* *backtrack-counter* *depth*)
    #+NIL
    (loop for index from 1 below 82
       as count = (aref *depth-counts* index)
       until (zerop count)
       do (format t "~&  Depth ~2D: ~:D~%" index count)))
  
  (cond
    ((every #'zerop (first sets))
     (assert (every #'zerop (second sets)))
     (assert (every #'zerop (third sets)))
     (print (list :finished prev-moves))
     (print (puzzle-array prev-moves))
     (finish-output)
     ;;(break "Found a solution ~A" (puzzle-array prev-moves))
     (throw 'finished (list prev-moves (puzzle-array prev-moves)))
     nil)
    (t
     ;; Find a move..
     (destructuring-bind (col row choices) (first (sorted-choices sets))
       (dotimes (bit 9)

	    (unless (zerop (logand (ash 1 bit) choices))

	      (solve (mark sets (list col row (1+ bit)))
		     (cons (list col row (1+ bit)) prev-moves)))))
     (incf *backtrack-counter*))))

(defun solve-puzzle (puzzle)
  (let ((*solve-counter* 0)
	(*backtrack-counter* 0)
	(*depth-counts* (make-array 82 :initial-element 0)))
    (catch 'finished
      (solve (apply-moves puzzle)
	     (reverse puzzle)))
    (format t "~&Called SOLVE ~:D times, backtracked ~:D times.~%" *solve-counter* *backtrack-counter*)))
