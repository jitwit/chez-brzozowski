(import (charset)
        (chezscheme))

;;;; derivatives with extended regex.

;;; null strings, empty strings, charsets, concatenation, conjunction,
;;; disjunction, negation, completion.

(define (unicode->char uxxxx)
  (integer->char (string->number uxxxx 16)))

(define (unicode->symbol uxxxx)
  (string->symbol (string (unicode->char uxxxx))))

(define re:null
  (unicode->symbol "2205"))

(define (re:null? R)
  (eq? R re:null))

(define re:empty
  (unicode->symbol "25B"))

(define (re:empty? R)
  (eq? R re:empty))

(define (re:non-empty? R)
  (not (re:empty? R)))

(define (re:non-null? R)
  (not (re:null? R)))

(define (re:set? re)
  (charset? re))

(define (tag tag R)
  (cons tag R))

(define (get-tag tagged-datum)
  (car tagged-datum))

(define (get-datum tagged-datum)
  (cdr tagged-datum))

(define (re:atom? R)
  (or (re:null? R)
      (re:empty? R)
      (char? R)
      (charset? R)))

(define (re:. . rs)
  (let ((rs (filter re:non-empty? rs)))
    (let ((nulls (filter re:null? rs)))
      (cond ((not (null? nulls)) re:null)
            ((null? rs) re:empty)
            ((= 1 (length rs)) (car rs))
            (else (tag 'seq rs))))))

(define (re:& . rs)
  (let ((nulls (filter re:null? rs)))
    (cond ((not (null? nulls)) re:null)
          ((null? rs) re:empty)
          ((= 1 (length rs)) (car rs))
          (else (tag '& rs)))))

(define (re:+ . rs)
  (let ((rs (filter re:non-null? rs)))
    (cond ((null? rs) re:null)
          ((= 1 (length rs)) (car rs))
          (else (tag '+ rs)))))

(define (re:* R)
  (tag '* R))

(define (re:string R)
  (apply re:. (string->list R)))

(define (<&> x y)
  (if (re:null? x)
      x
      y))

(define (<+> x y)
  (if (re:empty? x)
      x
      y))

(define (<-> x)
  (if (re:empty? x)
      re:null
      re:empty))

(define (nullify R)
  (if (re:atom? R)
      (cond ((re:null? R) re:null)
            ((re:empty? R) re:empty)
            ((char? R) re:null)
            (else (if (charset-empty? R)
                      re:empty
                      re:null)))
      (let ((tag (get-tag R))
            (sub (get-datum R)))
        (case tag
          ((*) re:empty)
          ((seq) (fold-left <&> re:empty (map nullify sub)))
          ((&) (fold-left <&> (nullify (car sub)) (map nullify (cdr sub))))
          ((+) (fold-left <+> (nullify (car sub)) (map nullify (cdr sub))))
          ((-) (<-> (nullify sub)))))))

(define (derive-atom x R)
  (cond ((char? R)
         (if (char=? x R)
             re:empty
             re:null))
        ((charset? R)
         (if (charset-member? x R)
             re:empty
             re:null))
        (else re:null)))

(define (derive x)
  (lambda (R)
    (if (re:atom? R)
        (derive-atom x R)
        (let ((tag (get-tag R))
              (sub (get-datum R)))
          (case tag
            ((seq)
             (re:+ (apply re:. ((derive x) (car sub)) (cdr sub))
                   (re:. (nullify (car sub))
                         ((derive x) (apply re:. (cdr sub))))))
            ((+) (apply re:+ (map (derive x) sub)))
            ((&) (apply re:& (map (derive x) sub)))
            ((*) (re:. ((derive x) sub) R))
            ((-) (re:- ((derive x) sub)))
            (else (error 'derive "todo/fixme" R x tag sub)))))))

(define (D s r)
  (fold-left (lambda (r* x)
               ((derive x) r*))
             r
             (string->list s)))

(define (re:match s r)
  (nullify (D s r)))

(define lang-cadr
  (re:. #\c
        (re:* (re:+ #\a #\d))
        #\r))

(define lang-loop
  (re:*
   (re:+ (re:string "for")
         (re:string "while")
         (re:string "do"))))

(define ab*
  (re:. #\a (re:* #\b)))
