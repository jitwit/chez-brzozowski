(print-gensym #f)

(define libraries-loaded #f)

(unless libraries-loaded
  (set! libraries-loaded #t)
  (library-directories (cons* "../chez-charset"
                              "."
                              (library-directories))))

(import (charset)
        (brzozowski))


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
  (re:charset "abcdefghijklmnopqrstuvwxyz"))

(define lang-ascii-letter
  (re:charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

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

(define lang-float
  (re:. (re:? (re:+ #\+ #\-))
        (re:+ (re:1+ (re:& lang-digit (re:- #\0)))
              #\0)
        (re:? (re:. #\. (re:1+ lang-digit)))))

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

(define lang-pathological-regexp-1
  (re:. (re:1+ (re:+ #\a
                     (re:string "aa")))
        #\b))

(define lang-pathological-regexp-2
  (re:+ (re:* (re:* #\a))
        (re:* #\b)))
