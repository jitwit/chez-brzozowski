(import (charset))

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

(define (tag tag datum)
  (cons tag datum))

(define (get-tag tagged-datum)
  (car tagged-datum))

(define (get-datum tagged-datum)
  (cdr tagged-datum))

(define (re:seq? re)
  (eq? (get-tag re) '..))

(define (re:and? re)
  (eq? (get-tag re) '&))

(define (re:or? re)
  (eq? (get-tag re) '+))

(define (re:set? re)
  (charset? re))

(define (re:star? re)
  (eq? (get-tag re) '*))

(define (re:neg? re)
  (eq? (get-tag re) '-))

(define (re:- re)
  (if (and (pair? re)
           (re:neg? re))
      (get-datum re)
      (tag '- re)))

(define (re:. re0 . res)
  (let ((res (filter (lambda (r)
                       (not (re:empty? r)))
                     (cons* re0 res))))
    ;; check for nulls
    (cond ((ormap re:null? res) re:null)
          ;; if only one, don't wrap
          ((= 1 (length res)) (car res))
          ;; otherwise wrap
          (else (tag 'seq res)))))

(define (re:* re)
  (if (or (re:null? re)
          (re:empty? re))
      re
      (tag '* re)))

(define (re:& re0 . res)
  (let ((res (cons re0 res)))
    (cond ((ormap re:null? res) re:null)
          ((null? res) (error 're:& "figure out what to do" re0 res))
          ((= 1 (length res)) (car res))
          (else (tag '& res)))))

(define (re:+ re0 . res)
  (let ((res (filter (lambda (x)
                       (not (re:null? x)))
                     (cons re0 res))))
    (cond ((null? res) re:null)
          ((= 1 (length res)) (car res))
          (else (tag '+ res)))))

(define (chars . chars)
  (list->charset chars))

(define (re:string s)
  (apply re:. (map chars (string->list s))))

(define (nullable? re)
  (cond ((not (pair? re))
         (cond ((re:null? re) #f)
               ((re:empty? re) #t)
               ((re:set? re) (charset-empty? re))
               (else (error 'nullable? "fixme1" re))))
        ((re:seq? re)
         (andmap nullable? (get-datum re)))
        ((re:and? re)
         (andmap nullable? (get-datum re)))
        ((re:or? re)
         (ormap nullable? (get-datum re)))
        ((re:star? re)
         #t)
        ((re:neg? re)
         (not (nullable? (get-datum re))))
        (else (error 'nullable? "fixme2" re))))

(define (re:nullable? re)
  (if (nullable? re)
      re:empty
      re:null))

(define (re:d x)
  (lambda (R)
    (if (not (pair? R))
        (if (or (re:null? R)
                (re:empty? R))
            re:null
            (if (charset-member? x R)
                re:empty
                re:null))
        (let ((tag (get-tag R))
              (reg (get-datum R)))
          (case tag
            ((*) (re:. ((re:d x) reg) (re:* reg)))
            ((&) (apply re:& (map (re:d x) reg)))
            ((+) (apply re:+ (map (re:d x) reg)))
            ((-) (re:- ((re:d x) reg)))
            ((seq)
             (re:+ (apply re:.
                          ((re:d x) (car reg))
                          (cdr reg))
                   (re:. (re:nullable? (car reg))
                         (apply re:. (cdr reg)))))
            (else (error 're:d "fixme" x R)))))))

(define (re:derive s R)
  (fold-left (lambda (R x)
               ;;               (format #t "~%~a ~a~%" R x)
               ((re:d x) R))
             R
             (string->list s)))

(define (re:match s R)
  (nullable? (re:derive s R)))

(define example
  (re:& (re:- (re:string "do"))
        (re:+ (re:string "for")
              (re:string "while")
              (re:string "do"))))

(define lang-cadr
  (re:. (chars #\c)
        (re:* (re:+ (chars #\a) (chars #\d)))
        (chars #\r)))
