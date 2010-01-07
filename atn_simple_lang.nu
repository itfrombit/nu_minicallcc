(load "atn.nu")

;; Simple language ATN example
(function types (w)
     (cdr (assoc w '((spot noun) (runs verb)))))

(defnode s
     (cat noun s2
          (setr subj g-atn-cur)))

(defnode s2
     (cat verb s3
          (setr v g-atn-cur)))

(defnode s3
     (up `(sentence
                   (subject ,(getr subj))
                   (verb ,(getr v)))))

(with-parses s '(spot runs)
     (puts "Parsing:" parse))




