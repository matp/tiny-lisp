(defun for (from to func)
  (cond ((< from to) (func from) (for (+ from 1) to func))))

(setq mandelbrot-chars
  (list " " "." "," "`" "'" "\"" ":" ";" "-" "+" "o" "O" "0" "1"
        "2" "3" "4" "5" "6" "7" "8" "9" "%" "*" "&" "$" "@" "#"))

(defun mandelbrot-iter (x y x0 y0 i)
  (cond ((= i 28) " ")
        ((> (+ (* x0 x0) (* y0 y0)) 4) (nth i mandelbrot-chars))
        (t (mandelbrot-iter x y
                            (+ (- (* x0 x0) (* y0 y0)) x) (+ (* 2 x0 y0) y)
                            (+ i 1)))))

(defun mandelbrot-char (x y)
  (mandelbrot-iter x y
                   (+ (- (* x x) (* y y)) x) (+ (* 2 x y) y)
                   0)) 

(defun mandelbrot (xmin xmax ymin ymax)
  (for 0 24 (lambda (py)
    (for 0 80 (lambda (px)
      (princ
        (mandelbrot-char
          (+ (* (/ px 80) (- xmax xmin)) xmin)
          (+ (* (/ py 24) (- ymax ymin)) ymin)))))
    (princ "\n"))))

(mandelbrot -2.15 1.25 -1.25 1.25)
; (mandelbrot -1.5 -1.1 -0.2 0.1)
