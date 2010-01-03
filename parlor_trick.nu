(load "Nutils:cl_utils")
(load "callcc.nu")
(load "choose.nu")

(puts (macrox
           (choose (+ x 2) (* x 2))
           ))

(function doto (x)
     (choose (+ x 2) (* x 2)))

(puts (doto 3))

(cc-function two-numbers ()
     (choose-bind  n1 '(0 1 2 3 4 5)
          (choose-bind n2 '(0 1 2 3 4 5)
               (cc-values n1 n2))))

(cc-function parlor-trick (sum)
     (cc-bind (n1 n2) (two-numbers)
              (if (== (+ n1 n2) sum)
                  (then
                       (choose-clear-paths)
                       `(the sum of ,n1 ,n2))
                  (else
                       (fail)))))

(puts (parlor-trick 7))

