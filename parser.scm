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

(define rien
  re:null)

(define tout
  (tag '* (tag '- rien)))

(define (no-good? R)
  (eq? R rien))

(define (re:everything? R)
  (equal? R tout))

(define (organize rs)
  (let ((table (make-hashtable equal-hash equal?)))
    (for-each (lambda (r)
                (hashtable-set! table r #t))
              rs)
    (vector->list
     (hashtable-keys table))))

;; nub and flatten based on tag
(define (flatten-like tag)
  (lambda (rs)
    (if (not (list? rs))
        (list rs)
        (fold-right (lambda (r xs)
                      (if (and (pair? r)
                               (eq? (get-tag r) tag))
                          (append ((flatten-like tag) (get-datum r))
                                  xs)
                          (cons r xs)))
                    '()
                    (organize rs)))))

(define (re:. . rs)
  (let ((rs (filter re:non-empty? rs)))
    (let ((nulls (filter re:null? rs)))
      (cond ((not (null? nulls)) re:null)
            ((null? rs) re:empty)
            ((= 1 (length rs)) (car rs))
            (else (tag 'seq rs))))))

(define (re:& . rs)
  (let ((rs ((flatten-like '&) rs)))
    (let ((nulls (filter re:null? rs)))
      (cond ((not (null? nulls)) re:null)
            ((null? rs) re:empty)
            ((= 1 (length rs)) (car rs))
            (else (tag '& rs))))))

(define (re:+ . rs)
  (let ((rs ((flatten-like '+) rs)))
    (if (memp re:everything? rs)
        tout
        (let ((rs (filter re:non-null? rs)))
          (cond ((null? rs) re:null)
                ((= 1 (length rs)) (car rs))
                (else (tag '+ rs)))))))

(define (re:* R)
  (cond ((re:atom? R) (tag '* R))
        ((eq? (get-tag R) '*) R)
        (else (tag '* R))))

(define (re:- R)
  (cond ((and (pair? R) (eq? '- (get-tag R)))
         (get-datum R))
        ((re:null? R) re:empty)
        ((re:empty? R) re:null)
        (else (tag '- R))))

(define (re:string R)
  (apply re:. (string->list R)))

(define (re:1+ r)
  (re:. r (re:* r)))

(define (re:? r)
  (re:+ re:empty r))

(define (<&> x y)
  (if (re:null? x)
      x
      y))

(define (<+> x y)
  (if (re:empty? x)
      x
      y))

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
          ((-) (re:- (nullify sub)))))))

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
            ((*) (re:. ((derive x) sub)
                       R))
            ((-) (re:- ((derive x) sub)))
            (else (error 'derive "fixme" R x tag sub)))))))

(define (memo-derive)
  (let ((table (make-hashtable equal-hash equal?)))
    (lambda (x R)
      (let ((previously? (hashtable-ref table (cons x R) #f)))
        (or previously?
            (let ((result ((derive x) R)))
              (hashtable-set! table (cons x R) result)
              result))))))

(define (D s r)
  (fold-left (lambda (r* x)
               ((derive x) r*))
             r
             (string->list s)))

(define (simple-match s r)
  (nullify (D s r)))

(define memo-match
  (let ((D* (memo-derive)))
    (lambda (s r)
      (nullify (fold-left (lambda (r x)
                            (D* x r))
                          r
                          (string->list s))))))

(define (make-memoized-matcher)
  (let ((D* (memo-derive)))
    (lambda (s r)
      (nullify (fold-left (lambda (r x)
                            (D* x r))
                          r
                          (string->list s))))))

;; return the first accepting index in S starting from j
(define (make-index-matcher L)
  (let ((D* (memo-derive)))
    (lambda (j S)
      (let ((n (string-length S))
            (end #f)
            (lang L))
        (do ((j j (1+ j)))
            ((or (= j n) end (no-good? lang)) end)
          (set! lang (D* (string-ref S j) lang))
          (when (re:empty? (nullify lang))
            (set! end (1+ j))))))))

(define (find-matches lang S)
  (let ((matcher (make-index-matcher lang))
        (n (string-length S)))
    (let loop ((a 0) (matches '()))
      (if (= a n)
          matches
          (let ((b (matcher a S)))
            (if (not b)
                (loop (1+ a) matches)
                (loop b (cons (cons a b) matches))))))))

(define (count-matches lang S)
  (length (find-matches lang S)))

(define lang-cadr
  (re:. #\c
        (re:1+ (re:+ #\a #\d))
        #\r))

(define lang-ab*
  (re:. #\a (re:* #\b)))

(define lang-ab*+c*ad
  (re:+ lang-ab*
        (re:. (re:* #\c)
              #\a
              #\d)))

(define lower-ascii
  (string->charset "abcdefghijklmnopqrstuvwxyz"))

(define lang-ascii-letter
  (string->charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(define lang-word
  (re:1+ lower-ascii))

(define lang-digit
  (string->charset "0123456789"))

(define lang-number
  (re:1+ lang-digit))

(define lang-whitespace
  (list->charset '(#\tab #\space #\newline)))

(define lang-1+1
  (re:. #\1 (re:* (re:. #\+ #\1))))

(define lang-n+n
  (re:. lang-number
        (re:* (re:. #\+ lang-number))))

(define lang-arith
  (let ((op (re:. (re:* lang-whitespace)
                  (string->charset "+*-/^%")
                  (re:* lang-whitespace))))
    (re:. lang-float
          (re:* (re:. op lang-float)))))

(define lang-loop
  (re:* (re:+ (re:string "for")
              (re:string "while")
              (re:string "if")
              (re:string "do")
              (re:. lang-word #\.))))

(define lang-float
  (re:. (re:? (re:+ #\+ #\-))
        (re:+ (re:1+ (re:& lang-digit (re:- #\0)))
              #\0)
        (re:? (re:. #\. (re:1+ lang-digit)))))

(define (show-matches lang S)
  (map (lambda (m)
         (substring S (car m) (cdr m)))
       (find-matches lang S)))
