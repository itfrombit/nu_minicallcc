(load "prolog.nu")

(<- (eats ?x ?f) (glutton ?x))
(<- (glutton horse))

(with-inference (eats ?x hay)
     (puts "with-inference glutton test: #{?x}"))

(with-inference (eats ?x ?y)
     (puts "with-inference glutton-wildcard test: #{?x} #{?y}"))

