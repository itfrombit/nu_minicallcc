(set g-cc-cont (do (x) x))

(macro cc-do (params *body)
     `(do (g-cc-cont ,@params) ,@body))

(macro cc-function (name params *body)
     (let ((__f ((+ "cc-" name) symbolValue)))
          (set __m (list __f 'g-cc-cont))
          (if (> (params length) 0)
              (then (set __m (append __m (list (list 'quasiquote-eval params))))))
          (set __m (list 'quasiquote __m))
          (puts __m)
          `(progn
                 (macro ,name ,params
                      ,__m)
                 (function ,__f (g-cc-cont ,@params) ,@*body))))

(macro cc-bind (params expr *body)
     `(let ((g-cc-cont (do ,params ,@*body))) ,expr))

(macro cc-values (*vals)
     `(g-cc-cont ,@*vals))

(macro cc-apply (fn *params)
     `(apply ,fn g-cc-cont ,@*params))


;; Utility functions for a simple stack
(macro push (v l)
     `(set ,l (cons ,v ,l)))

(macro pop (l)
     `(progn
            (cond ((null? ,l) nil)
                  (else
                       (set __v (car ,l))
                       (set ,l (cdr ,l))
                       __v))))

