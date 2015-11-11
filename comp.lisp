(defun f1 (ac2 xz2 ac3 xz3)
  ( + ( * 4 ac2 xz2) (* 12 ac2 xz3) (* 12 ac3 xz2) (* 27 ac3 xz3) ) )
(defun objective ( a c x z)
  ( f1
   ( + ( * a a ) (* a c ) (* c c) )
   ( + ( * x x ) (* x z ) (* z z) )
   ( * a c (+ a c ) )
   ( * x z (+ x z ) )))

(defun constraints (eps)
  (lambda (a c x z) ( and
    (leq a 0 c)
    (leq x 0 z)
    (leq (* -2 c) a (* -0.5 c))
    (leq (* -2 z) x (* -0.5 z))
    (leq (- c x) (+ 1 (- (/ 1 3) (* 4 eps))))
    (leq (- z a) (+ 1 (- (/ 1 3) (* 4 eps))))
    (leq (+ a x) (+ (/ -2 3) (* 2 eps)))
    (leq (+ c z) (- (/ 4 3) (* 4 eps)))
    (leq (+ 1 a) z))))

(defun range () 
  '((a -10 0)
   (c 0 10)
   (z 0 10)
   (x -10 0)))

( (constraints 0.1) .1 .1 .1 .1)
;(defun check-constraint (vars eq)
