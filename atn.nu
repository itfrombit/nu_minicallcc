;; Augmented Transition Networks using continuations.

(load "Nutils:cl_utils")
(load "choose.nu")

(set g-atn-cur nil)

(macro defnode (name *arcs)
     `(cc-function ,name (pos regs) (choose ,@*arcs)))

(macro down (sub next *cmds)
     `(cc-bind (g-atn-cur pos regs) (,sub pos (cons nil regs))
               (,next pos ,(compile-cmds *cmds))))

(macro cat (cat next *cmds)
     `(if (== ($sent length) pos)
          (then
               (fail))
          (else
               (let ((g-atn-cur (nth pos $sent)))
                    (if (member ',cat (types g-atn-cur))
                        (then
                             (,next (1+ pos) ,(compile-cmds *cmds)))
                        (else
                             (fail)))))))

(macro jump (next *cmds)
     `(,next pos ,(compile-cmds *cmds)))

(function compile-cmds (cmds)
     (if (null? cmds)
         (then 'regs)
         (else
              `(,@(car cmds) ,(compile-cmds (cdr cmds))))))

(macro up (expr)
     `(let ((g-atn-cur (nth pos $sent)))
           (cc-values ,expr pos (cdr regs))))

(macro getr (key *regs)
     (if (null? *regs) ;; regs is optional
         (then (set __regs 'regs))  ;; default value
         (else (set __regs (car regs))))
     `(let ((result (cdr (assoc ',key (car ,__regs)))))
           (if (cdr result)
               (then result)
               (else (car result)))))

(macro set-register (key val regs)
     `(cons (cons (cons ,key ,val) (car ,regs))
            (cdr ,regs)))

(macro setr (key val regs)
     `(set-register ',key (list ,val) ,regs))

(macro pushr (key val regs)
     `(set-register ',key
           (cons ,val (cdr (assoc ',key (car ,regs))))
           ,regs))

(macro with-parses (node sent *body)
     `(progn
            (set $sent ,sent)
            (set $paths nil)
            (cc-bind (parse __pos __regs) (,node 0 '(nil))
                     (if (== __pos ($sent length))
                         (then (progn ,@*body (fail)))
                         (else (fail))))))

