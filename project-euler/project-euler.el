(require 'cl)

(defun euler-p-001 ()
  "Find the sum of all the multiples of 3 or 5 below 1000."
  (loop with result = 0
        for i from 1 to 999
        do
        (progn
          (if (or (= (% i 3) 0)
                  (= (% i 5) 0))
              (incf result i)))
        finally
        (return result)))

