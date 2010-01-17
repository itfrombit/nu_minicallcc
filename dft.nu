(load "Nutils:cl_utils")
(load "callcc.nu")

;; A test generator example:

(function dft (tree)
     (cond ((null? tree) nil)
           ((atom tree) (puts tree))
           (else (dft (car tree))
                 (dft (cdr tree)))))

(set g-dft-saved nil)

(=function dft-node (tree)
     (cond ((null? tree)
            (restart))
           ((atom tree)
            (=values tree))
           (else
                (push (do () (dft-node (cdr tree))) g-dft-saved)
                (dft-node (car tree)))))

(=function restart ()
     (if (not (null? g-dft-saved))
         (then
              (set f (pop g-dft-saved))
              (f))
         (else
              (=values "done"))))

(=function dft2 (tree)
     (set g-dft-saved nil)
     (=bind (node) (dft-node tree)
            (cond ((== node "done") (=values nil))
                  (else (puts node)
                        (restart)))))


(set t1 '(10 (20 (30 40)) (50 60 (70 80) 90)))
(set t2 '(1 (2 (3 6 7) 4 5)))

(puts "(dft-node t2): #{(dft-node t2)}")
(puts "    (restart): #{(restart)}")
(puts "  g-dft-saved: #{g-dft-saved}")

(puts "(dft2 t2): #{(dft2 t2)}")


(puts "Generating all combinations of the tree nodes:")
(puts
     (=bind (node1) (dft-node t1)
            (if (== node1 'done)
                (then 'done)
                (else
                     (=bind (node2) (dft-node t2)
                            (list node1 node2))))))

(30 times: (do (x) (puts (restart))))



;(dft2 t2)

;; A few quick tests...
;(macrox (=function add1 (x) (=values (+ 1 x))))
;
;(=function add1 (x) (=values (+ 1 x)))
;
;(add1 5)

;; Multiple args
;(macrox (=function a (b c) (+ b c)))
;
;(=function a (b c) (+ b c))
;
;(a 1 2)


