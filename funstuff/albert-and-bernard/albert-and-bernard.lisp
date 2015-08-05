;;;; http://nbviewer.ipython.org/url/norvig.com/ipython/Cheryl.ipynb

;; When is Cheryl's Birthday?
;; Peter Norvig, April 2015
;; This logic puzzle has been making the rounds:

;; Albert and Bernard just became friends with Cheryl, and they want
;; to know when her birthday is. Cheryl gave them a list of 10
;; possible dates:

;; May 15     May 16     May 19
;; June 17    June 18
;; July 14    July 16
;; August 14  August 15  August 17

;; Cheryl then tells Albert and Bernard separately the month and the
;; day of the birthday respectively.

;; Albert: I don't know when Cheryl's birthday is, but I know that
;; Bernard does not know too.

;; Bernard: At first I don't know when Cheryl's birthday is, but I
;; know now.

;; Albert: Then I also know when Cheryl's birthday is.

(defparameter *dates*
    '(         (may 15) (may 16)                   (may 19)
                                 (jun 17) (jun 18)
      (jul 14)          (jul 16)
      (aug 14) (aug 15)          (aug 17)))

(defun month-of (x) (first x))
(defun day-of (x) (second x))

(defun known (possibilities)
  (= 1 (length possibilities)))

(defun given (fact &key (among *dates*))
  (remove-if-not (lambda (x) (member fact x)) among))

(defun statement-3 (her-birthday)
  (let ((possible (given (month-of her-birthday))))
    (and (not (known possible))
         (every (lambda (date) (not (known (given (day-of date)))))
                possible))))

(defun statement-4 (her-birthday)
  (and (not (known (given (day-of her-birthday))))
       (known (remove-if-not #'statement-3 (given (day-of her-birthday))))))

(defun statement-5 (her-birthday)
  (known (given (month-of her-birthday) :among (remove-if-not 'statement-4 *dates*))))

(defun find-her-birthday ()
  (remove-if-not
   (lambda (date) (and (statement-3 date) (statement-4 date) (statement-5 date)))
   *dates*))
