
(defun f1 (ac2 xz2 ac3 xz3)
  ( + ( * 4 ac2 xz2) (* 12 ac2 xz3) (* 12 ac3 xz2) (* 27 ac3 xz3) ) )
(defun objective ( a c x z)
  ( f1
   ( + ( * a a ) (* a c ) (* c c) )
   ( + ( * x x ) (* x z ) (* z z) )
   ( * a c (+ a c ) )
   ( * x z (+ x z ) )))

(defun constraints ()
  '(
    (leq a 0 c)
    (leq x 0 z)
    (leq (* -2 c) a (* -0.5 c))
    (leq (* -2 z) x (* -0.5 z))
    (leq (- c x) (+ 1 (- (/ 1 3)
    


			     


