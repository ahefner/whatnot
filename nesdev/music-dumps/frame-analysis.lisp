;;;; Scratchpad for experimenting with compression schemes for NES music dumps

(defun flatten-register-dump (frame)
  (loop with regs = (make-array 24 :initial-element nil)
        for (_ reg val) in (rest frame)
        do (setf (aref regs reg) val)
        finally (return regs)))


