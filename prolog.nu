;; Simple prolog interpreter
(load "Nu:math")
(load "choose.nu")

(set $debug nil)

(macro dbg (*args)
     (if $debug
         (then `(puts ,@*args))
         (else nil)))

(macro multiple-value-bind (vars vals *body)
     (set i 0)
     (set __evalvals (eval vals))
     ;(puts "mvb: #{__evalvals}")
     (set letvars nil)
     (while (< i (vars length))
            (set letvars (append letvars (list (list (nth i vars)
                                                     `',(nth i __evalvals)))))
            (set i (+ i 1)))
     ;(puts "mvb letvars: #{letvars}")
     `(let ,letvars
           ,@*body))

;(multiple-value-bind (a b) (binding '?x b) (list a b))

(macro aif (test-clause then-clause else-clause)
     `(let ((it ,test-clause))
           (if it
               (then ,then-clause)
               (else ,else-clause))))

(macro aif2 (test-clause then-clause else-clause)
     `(multiple-value-bind (it __win) ,test-clause
           (if (or it __win)
               (then ,then-clause)
               (else ,else-clause))))


(macro acond2 (*clauses)
     (dbg "acond2: clauses: #{*clauses}")
     (if (null? *clauses)
         (then nil)
         (else
              (let ((cl1 (car *clauses)))
                   (dbg "acond2-let: cl1: #{cl1}")
                   `(multiple-value-bind (__val __win) ,(car cl1)
                         (if (or (not (null? __val)) (not (null? __win)))
                             (then
                                  (let ((it __val))
                                       ,@(cdr cl1)))
                             (else
                                  (acond2 ,@(cdr *clauses)))))))))

(macro apply-key (key item)
     `(if ,key
          (then (,key ,item))
          (else ,item)))

(function sublis (alist tree)
     (cond
          ((null? tree) nil)
          ((atom tree)
           (if (assoc tree alist)
               (then (cadr (assoc tree alist)))
               (else tree)))
          (else
               (cons (sublis alist (car tree)) (sublis alist (cdr tree))))))


(function vars-in (expr)
     (if (atom expr)
         (then
              (if (varsym? expr)
                  (then (list expr))))
         (else
              (union (vars-in (car expr))
                     (vars-in (cdr expr))))))


(function union (l1 l2)
     (set r nil)
     ((list l1 l2) map:
      (do (l)
          (set i 0)
          (while (< i (l length))
                 (if (not (member (nth i l) r))
                     (then (push (nth i l) r)))
                 (incf i))
          ))
     r)


;;;

(function gensym (*x)
     (set s (+ "G" ((floor (rand 999999999)))))
     (if (and (not (null? *x) (not (null? (car *x)))))
         (then (set s (+ ((car *x) stringValue) s))))
     (s symbolValue))

(function rep_ (x)
     (if (atom x)
         (then
              (if (== x '_)
                  (then (gensym "?"))
                  (else x)))
         (else
              (cons (rep_ (car x)) (rep_ (cdr x))))))


(function binding-rec (x binds)
     (aif (assoc x binds)
          (or (binding-rec (cdr it) binds) it)
          nil))

(function binding (x binds)
     ;(dbg "in binding: x: #{x}")
     (let ((b (binding-rec x binds)))
          ;(dbg "in binding: b: #{b}")
          ;(dbg "            (cadr b): #{(cadr b)}")
          (if (or (null? b) (null? (cdr b)))
              (then (list nil b))
              (else
                   (list (cadr b) b)))))

(function fullbind (x b)
     ;(dbg "in fullbind: #{x}")
     ;(dbg "             #{b}")
     (cond
          ((null? x)
           ;(dbg "in fullbind-null?")
           nil)
          ((varsym? x)
           ;(dbg "in fullbind-varsym: #{x}")
           (aif2 (binding x b)
                 (progn
                       ;(dbg "in fullbind-aif-then: #{it}")
                       (fullbind it b))
                 (progn
                       ;(dbg "in fullbind-aif-else: #{it}")
                       (gensym))))
          ((atom x)
           ;(dbg "in fullbind-atom: #{x}")
           x)
          (else
               ;(dbg "in fullbind-t: (car x): #{(car x)}")
               ;(dbg "in fullbind-t: (cdr x): #{(cdr x)}")
               (set l (fullbind (car x) b))
               (set r (fullbind (cdr x) b))
               ;(dbg "in fullbind-t: l: #{l}")
               ;(dbg "in fullbind-t: r: #{r}")
               (if (or (null? (cdr x)) (null? (cadr x)))
                   (then
                        (fullbind (car x) b))
                   (else
                        (cons (fullbind (car x) b)
                              (fullbind (cadr x) b)))))))

(function varsym? (x)
     (and
         (symbol? x)
         (== "?" (subseq (x stringValue) 0 1))))


(cc-function prove-query (expr binds)
     (dbg "prove-query: expr: #{expr}")
     (dbg "prove-query: binds: #{binds}")
     (case (car expr)
           ('and  (dbg "prove-query: and")  (prove-and (cdr expr) binds))
           ('or   (dbg "prove-query: or")   (prove-or (cdr expr) binds))
           ('not  (dbg "prove-query: not")  (prove-not (car (cdr expr)) binds))
           (else (dbg "prove-query: else") (prove-simple expr binds))))


(cc-function prove-and (clauses binds)
     (if (null? clauses)
         (then
              (cc-values binds))
         (else
              (cc-bind (binds)
                       (prove-query (car clauses) binds)
                       (prove-and (cdr clauses) binds)))))

(cc-function prove-or (clauses binds)
     (choose-bind c clauses
          (prove-query c binds)))

(cc-function prove-not (expr binds)
     (let ((save-paths $paths))
          (set $paths nil)
          (choose (cc-bind (b) (prove-query expr binds)
                           (set $paths save-paths)
                           (fail))
                  (progn
                        (set $paths save-paths)
                        (cc-values binds)))))

(cc-function prove-simple (query binds)
     (dbg "prove-simple: query: #{query}")
     (dbg "prove-simple: binds: #{binds}")
     (dbg "prove-simple: rlist: #{$rlist}")
     (choose-bind r $rlist
          (dbg "prove-simple: r: #{r}")
          (implies r query binds)))

(set $rlist nil)

(macro <- (con *ant)
     (dbg "<-: original *ant: #{*ant}")
     (let ((ant (if (== (*ant length) 1)
                    (then (car *ant))
                    (else `(and ,@*ant)))))
          (dbg "<-:  con: #{con}")
          (dbg "<-: ant: #{ant}")
          `((set $rlist (append $rlist (rep_ (list (cons ',ant ',con))))) length)))

(cc-function implies (r query binds)
     (dbg "implies: -----------------------")
     (dbg "implies:     r: #{r}")
     (dbg "implies: query: #{query}")
     (dbg "implies: binds: #{binds}")
     (let ((r2 (change-vars r)))
          (dbg "implies: r2: #{r2}")
          (aif2 (match query (cdr r2) binds)
                (progn
                      (dbg "implies-aif2-then: it: #{it}")
                      (prove-query (car r2) it))
                (progn
                      (dbg "implies-aif2-else")
                      (fail)))))

(function change-vars (r)
     (sublis (mapcar-1 (do (v)
                           (list v (symb "?" ((gensym) stringValue))))
                  (vars-in r))
             r))

(macro with-inference (query *body)
     (dbg "with-inf: query: #{query}")
     (dbg "with-inf: *body: #{*body}")
     
     `(progn
            (set $paths nil)
            (cc-bind (binds) (prove-query ',(rep_ query) nil)
                     (let ,(mapcar-1 (do (v)
                                         `(,v (fullbind ',v binds)))
                                (vars-in query))
                          ,@*body
                          (fail)
                          )
                     )))

(function match (x y binds)
     (dbg "-------------------")
     (dbg "match:      x: #{x}")
     (dbg "match:      y: #{y}")
     (dbg "match:  binds: #{binds}")
     (cond
          ((or (== x y) (== x '_) (== y '_))
           (dbg "match-or")
           (list binds t))
          ((not (null? (car (binding x binds))))
           (dbg "match-binding x")
           (match (car (binding x binds)) y binds))
          ((not (null? (car (binding y binds))))
           (dbg "match-binding y")
           (match x (car (binding y binds)) binds))
          ((varsym? x)
           (dbg "match-varsym? x")
           (set res (list (cons (list x y) binds) t))
           (dbg "  #{res}")
           res)
          ((varsym? y)
           (dbg "match-varsym? y")
           (set res (list (cons (list y x) binds) t))
           (dbg "  #{res}")
           res)
          ((and (pair? x) (pair? y))
           (dbg "match-pair")
           (set match-result (match (car x) (car y) binds))
           (if (cadr match-result)
               (then (match (cdr x) (cdr y) (car match-result)))
               (else (list nil nil))))
          (else
               (dbg "match-else")
               (list nil nil))))


(<- (painter ?x) (and (hungry ?x) (smells-of ?x turpentine)))
(<- (hungry ?x) (or (gaunt ?x) (eats-ravenously ?x)))
(<- (gaunt raoul))
(<- (smells-of raoul turpentine))
(<- (painter rubens))

(puts "$rlist: #{$rlist}")


(with-inference (painter ?x)
     (puts "with-inference painter test:  #{?x}"))

(with-inference (gaunt ?x)
     (puts "with-inference gaunt test:  #{?x}"))


;(puts "Test sublis: #{(sublis '((x 1) (y 2)) '(+ (* x x) (* y y)))}")


