(load "Nutils:cl_utils")
(load "choose.nu")

(puts (macrox
           (choose (+ x 2) (* x 2))
           ))

(function doto (x)
     (choose (+ x 2) (* x 2)))


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

(parlor-trick 7)


(function kids (n)
     (case n
           (1 '(2 3))
           (2 '(4 5))
           (3 '(4 6))
           (6 '(7))))

;;(puts (macrox
(cc-function descent (n1 n2)
     (cond
          ((== n1 n2)
           (puts "n1 and n2 are equal")
           (cc-values (list n2)))
          ((kids n1)
           (choose-bind n (kids n1)
                (puts "n: #{n}   n1: #{n1}   n2: #{n2}")
                (cc-bind (p) (descent n n2)
                         (cc-values (cons n1 p)))))
          (else (fail))))
;;))

(descent 1 7)







