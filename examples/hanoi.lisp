(defun mapc1 (fn xs)
  (cond ((null xs) nil)
    (t (fn (car xs)) (mapc1 fn (cdr xs)))))

(defun hanoi-print (disk from to)
  (mapc1 princ (list "Move disk " disk " from " from " to " to "\n")))

(defun hanoi-move (n from to via)
  (cond ((= n 1)
         (hanoi-print n from to))
        (t
         (hanoi-move (- n 1) from via to)
         (hanoi-print n from to)
         (hanoi-move (- n 1) via to from))))

(defun hanoi (n)
  (hanoi-move n 'L 'M 'R))

(hanoi 3)
