(load "prolog.nu")

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

