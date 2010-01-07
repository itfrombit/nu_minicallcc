(load "Nutils:cl_utils")
(load "callcc.nu")

(set g-choose-paths nil)
(set g-choose-failsym '@)


(function fail ()
     (if g-choose-paths
         (then
              ((pop g-choose-paths)))
         (else
              g-choose-failsym)))

(macro choose (*choices)
     (if (not (null? *choices))
         (then
              `(progn
                     ,@((reverse (cdr *choices)) map:
                       (do (c)
                           `(push (do () ,c) g-choose-paths)))
                     ,(car *choices)))
         (else
              '(fail))))

(macro choose-bind (var choices *body)
     `(cb (do (,var) ,@*body) ,choices))

(function cb (fn choices)
     (if choices
         (then
              (if (cdr choices)
                  (push (do () (cb fn (cdr choices)))
                        g-choose-paths))
              ((fn (car choices))))
         (else
              (fail))))

(function choose-clear-paths ()
     (set g-choose-paths nil))


