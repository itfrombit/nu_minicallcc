(load "atn.nu")

(function types (word)
     (case word
           ('do         '(aux v))
           ('does       '(aux v))
           ('did        '(aux v))
           ('time       '(n v))
           ('times      '(n v))
           ('fly        '(n v))
           ('flies      '(n v))
           ('like       '(v prep))
           ('liked      '(v))
           ('likes      '(v))
           ('a          '(det))
           ('an         '(det))
           ('the        '(det))
           ('arrow      '(n))
           ('arrows     '(n))
           ('i          '(pron))
           ('you        '(pron))
           ('he         '(pron))
           ('she        '(pron))
           ('him        '(pron))
           ('her        '(pron))
           ('it         '(pron))
           (else nil)))


;; modifier string subnetwork
(defnode mods
     (cat n mods-n
          (setr mods g-atn-cur)))

(defnode mods-n
     (cat n mods-n
          (pushr mods g-atn-cur))
     (up `(n-group ,(getr mods))))


;; noun phrase subnetwork
(defnode np
     (cat det np-det
          (setr det g-atn-cur))
     (jump np-det
           (setr det nil))
     (cat pron pron
          (setr n g-atn-cur)))

(defnode pron
     (up `(np (pronoun ,(getr n)))))

(defnode np-det
     (down mods np-mods
           (setr mods g-atn-cur))
     (jump np-mods
           (setr mods nil)))

(defnode np-mods
     (cat n np-n
          (setr n g-atn-cur)))

(defnode np-n
     (up `(np (det ,(getr det))
              (modifiers ,(getr mods))
              (noun ,(getr n))))
     (down pp np-pp
           (setr pp g-atn-cur)))

(defnode np-pp
     (up `(np (det ,(getr det))
              (modifiers ,(getr mods))
              (noun ,(getr n))
              ,(getr pp))))


;; prepositional phrase subnetwork
(defnode pp
     (cat prep pp-prep
          (setr prep g-atn-cur)))

(defnode pp-prep
     (down np pp-np
           (setr op g-atn-cur)))

(defnode pp-np
     (up `(pp (prep ,(getr prep))
              (obj ,(getr op)))))


;; sentence network
(defnode s
     (down np s-subj
           (setr mood 'decl)
           (setr subj g-atn-cur))
     (cat v v
          (setr mood 'imp)
          (setr subj '(np (pron you)))
          (setr aux nil)
          (setr v g-atn-cur)))

(defnode s-subj
     (cat v v
          (setr aux nil)
          (setr v g-atn-cur)))

(defnode v
     (up `(s (mood ,(getr mood))
             (subj ,(getr subj))
             (vcl (aux ,(getr aux))
                  (v ,(getr v)))))
     (down np s-obj
           (setr obj g-atn-cur)))

(defnode s-obj
     (up `(s (mood ,(getr mood))
             (subj ,(getr subj))
             (vcl (aux ,(getr aux))
                  (v ,(getr v)))
             (obj ,(getr obj)))))


;; Test it out
(with-parses mods '(time arrow)
     (puts "Parsing: #{parse}"))

(with-parses np '(it)
     (puts "Parsing: #{parse}"))

(with-parses np '(arrows)
     (puts "Parsing: #{parse}"))

(with-parses np '(a time fly like him)
     (puts "Parsing: #{parse}"))

(with-parses s '(time flies like an arrow)
     (puts "Parsing: #{parse}"))









