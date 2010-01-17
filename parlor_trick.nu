(load "Nutils:cl_utils")
(load "callcc.nu")
(load "choose.nu")

(=function two-numbers ()
     (choose-bind  n1 '(0 1 2 3 4 5)
          (choose-bind n2 '(0 1 2 3 4 5)
               (=values n1 n2))))

(=function parlor-trick (sum)
     (=bind (n1 n2) (two-numbers)
            (if (== (+ n1 n2) sum)
                (then
                     ;(choose-clear-paths)
                     `(the sum of ,n1 ,n2))
                (else
                     (fail)))))


;; Test out choose with a simple example:
(function doto (x)
     (choose (+ x 2) (* x 2)))

(puts "(macrox (choose (+ x 2) (* x 2))):")
(puts (macrox
           (choose (+ x 2) (* x 2))
           ))

(puts "(choose (+ x 2) (* x 2))")

(puts "(doto 3): #{(doto 3)}")
(puts "(fail): #{(fail)}")

(choose-clear-paths)
(puts "(parlor-trick 7): #{(parlor-trick 7)}")
(puts "(fail): #{(fail)}")
(puts "(fail): #{(fail)}")
(puts "(fail): #{(fail)}")
(puts "(fail): #{(fail)}")

