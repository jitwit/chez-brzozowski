
(load "brzozowski.sls")
(import (brzozowski))

(assert (re:empty? (memo-match "a" #\a)))
(assert (re:null? (memo-match "b" #\a)))
(assert (re:empty? (memo-match "cat" (re:+ (re:string "dog") (re:string "cat")))))
(assert (re:empty? (memo-match "dog" (re:+ (re:string "dog") (re:string "cat")))))
(assert (re:null? (memo-match "bird" (re:+ (re:string "dog") (re:string "cat")))))
(assert (re:empty? (memo-match "" (re:* #\?))))
(assert (re:empty? (memo-match "???" (re:* #\?))))
(assert (re:null? (memo-match "???!" (re:* #\?))))
(assert (= 4 (length (find-matches #\a "babayaga"))))
(assert (equal? '("123" "234") (show-matches (re:1+ (re:charset "0123456789")) "123,234")))
