;;;; Letter Change / Word Morph puzzle solver?

;;; This solves puzzles of the sort where you are asked to find a
;;; sequence of words that translates one word into another by
;;; changing a single letter at a time such that you have a valid
;;; dictionary word at each intermediate step:

;;; CL-USER> (solve-word-morph 'house 'gorge)
;;; GORGE 
;;; GORSE 
;;; HORSE 
;;; HOUSE 

;;; Slighty awkward optimization - INTERN words as symbols (in whatever
;;; package, who cares) to optimize word graph generation. dict file is 100k
;;; words making naive word graph generation on order of 10^10 operations -
;;; slow enough (a few minutes) to be annoying, and using EQUAL hash tables
;;; doing string hashing seems like a gratuitous inefficiency.

;;; Oddly, while symbol + EQ hash is about twice as fast as strings +
;;; EQUAL hash, symbol + EQL hash was much, much slower. I'd expected
;;; the delta for the former to be larger, but it's still
;;; worthwhile. The latter (EQ vs EQL hash on symbols) is baffling.

;;; The word graph is built in quadratic time by calculating
;;; WORD-DISTANCE for every pair of words, and takes a minute or two
;;; on a 100k dictionary. It would probably be much faster to instead
;;; generate every potential neighboring word directly and add edges
;;; only for the words present in the dictionary. As such, this is all
;;; pretty silly in hindsight.

(defparameter *dictionary-file* "/usr/share/dict/american-english")

(defun load-words (&optional (filename *dictionary-file*))
  (with-open-file (in filename)
    (remove-duplicates
     (mapcar (lambda (x) (intern (string-upcase (remove #\' x))))
             (loop as line = (read-line in nil nil) while line collect line)))))

(defun word-length (word) (length (symbol-name word)))

(defun same-length (sym1 sym2)
  (= (word-length sym1) (word-length sym2)))

(defun word-distance (word1 word2)
  (unless (same-length word1 word2)
    (error "Words ~W and ~W must have equal length" word1 word2))
  (loop
     for c1 across (symbol-name word1)
     for c2 across (symbol-name word2)
     summing (if (char= c1 c2) 0 1)))

(assert (= 0 (word-distance 'blub 'blub)))
(assert (= 1 (word-distance 'blub 'blab)))
(assert (= 2 (word-distance 'blub 'blah)))

(defun similar? (word other)
  (and (same-length other word)
       (= 1 (word-distance other word))))

(defun similar-words (word words)
  (loop for other in words
     when (similar? other word)
     collect other))

(defun build-word-graph (words)
  "Given a list of words, build a graph of words of distance one apart, as
a hash table from the word to a list of neighboring words"
  (loop
     with graph = (make-hash-table :size 200000 :test 'eq)
     with max-length = (reduce #'max words :key #'word-length)
     for n from 0 upto max-length
     do
     ;; Minor (~3/4) optimization - runtime is still O(N^2), but reduce
     ;; 'N' via grouping words of the same length.
       (loop
          with words-of-length-n = (remove-if-not (lambda (w) (= n (word-length w))) words)
          for word in words-of-length-n
          do (setf (gethash word graph) (similar-words word words-of-length-n)))
     finally (return graph)))

(defvar *word-graph* nil)

(defun word-graph-memo ()
  (or *word-graph*
      (setf *word-graph* (build-word-graph (load-words)))))

(defun init-distance-map (word)
  (let ((map (make-hash-table :test 'eq :size 200000)))
    (setf (gethash word map) 0)
    map))

(defun solve-word-morph (start end &optional (graph (word-graph-memo)))
  (loop
     with distance-of = (init-distance-map start)
     for n upfrom 1
     as ntouched = 0
     as new-distance-of = (make-hash-table :test 'eq :size 200000)
     do
       (maphash
        (lambda (word neighbors)
          (when (gethash word distance-of)
            (setf (gethash word new-distance-of) (gethash word distance-of))
            (dolist (neighbor neighbors)
              (unless (gethash neighbor distance-of)
                (setf (gethash neighbor new-distance-of) n)
                (incf ntouched)))))
        graph)
       (setf distance-of new-distance-of)
     until (zerop ntouched)
     finally (blah end (gethash end distance-of) distance-of)))

(defun blah (next-word next-distance distance-of)
  (assert next-distance)
  (print next-word)
  (maphash
   (lambda (word distance)
     (when (and (= (1- next-distance) distance)
                (similar? word next-word))
       (return-from blah
         (cond
           ((zerop distance)
            (print word))
           (t (blah word (1- next-distance) distance-of))))))
   distance-of)
  (break "?"))


