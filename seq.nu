(load "Nutils:cl_utils")
(load "choose.nu")

(function kids (n)
     (case n
           (1 '(2 3))
           (2 '(4 5))
           (3 '(4 6))
           (6 '(7))
           (else nil)))


(cc-function descent (n1 n2)
     (cond
          ((== n1 n2)
           (cc-values (list n2)))
          ((kids n1)
           (choose-bind n (kids n1)
                (cc-bind (p) (descent n n2)
                         (cc-values (cons n1 p)))))
          (else (fail))))


(puts "(descent 1 7): #{(descent 1 7)}")
(puts "(descent 3 7): #{(descent 3 7)}")
(puts "(descent 1 8): #{(descent 1 8)}")


(puts "(descent 1 4): #{(descent 1 4)}")
(puts "(fail): #{(fail)}")
(puts "(fail): #{(fail)}")
