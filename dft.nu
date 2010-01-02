(load "callcc.nu")

;; A test generator example:

(function dft (tree)
     (cond ((null? tree) nil)
           ((atom tree) (puts tree))
           (else (dft (car tree))
                 (dft (cdr tree)))))

(set g-dft-saved nil)

(cc-function dft-node (tree)
     (cond ((null? tree)
            (restart))
           ((atom tree)
            (cc-values tree))
           (else
                (push (do () (dft-node (cdr tree))) g-dft-saved)
                (dft-node (car tree)))))

(cc-function restart ()
     (if (not (null? g-dft-saved))
         (then
              (set f (pop g-dft-saved))
              (f))
         (else
              (cc-values "done"))))

(cc-function dft2 (tree)
     (set g-dft-saved nil)
     (cc-bind (node) (dft-node tree)
              (cond ((== node "done") (cc-values nil))
                    (else (puts node)
                          (restart)))))


(set t1 '(10 (20 (30 40)) (50 60 (70 80) 90)))
(set t2 '(1 (2 (3 6 7) 4 5)))

;(dft t2)

;(puts "restart macro:")
;(puts restart)
;(puts "restart function:")
;(puts cc-restart)
;(puts "dft-node macro")
;(puts dft-node)
;(puts "dft-node function")
;(puts cc-dft-node)

(puts (dft-node t2))

(puts (restart))

(puts g-dft-saved)

(dft2 t2)

;; Generate all combinations of the tree nodes
(puts
     (cc-bind (node1) (dft-node t1)
              (if (== node1 'done)
                  (then 'done)
                  (else
                       (cc-bind (node2) (dft-node t2)
                                (list node1 node2))))))

(30 times: (do (x) (puts (restart))))



;(dft2 t2)

;; A few quick tests...
;(macrox (cc-function add1 (x) (cc-values (+ 1 x))))
;
;(cc-function add1 (x) (cc-values (+ 1 x)))
;
;(add1 5)

;; Multiple args
;(macrox (cc-function a (b c) (+ b c)))
;
;(cc-function a (b c) (+ b c))
;
;(a 1 2)


