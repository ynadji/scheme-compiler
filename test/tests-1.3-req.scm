


(add-tests-with-string-output "add1"
  [(add1 0) => "1\n"]
  [(add1 -1) => "0\n"]
  [(add1 1) => "2\n"]
  [(add1 -100) => "-99\n"]
  [(add1 1000) => "1001\n"]
  [(add1 536870910) => "536870911\n"]
  [(add1 -536870912) => "-536870911\n"]
  [(add1 (add1 0)) => "2\n"]
  [(add1 (add1 (add1 (add1 (add1 (add1 12)))))) => "18\n"]
  )

(add-tests-with-string-output "integer->char and char->integer"
   [(integer->char 65) => "#\\A\n"]
   [(integer->char 97) => "#\\a\n"]
   [(integer->char 122) => "#\\z\n"]
   [(integer->char 90) => "#\\Z\n"]
   [(integer->char 48) => "#\\0\n"]
   [(integer->char 57) => "#\\9\n"]
   [(char->integer #\A) => "65\n"]
   [(char->integer #\a) => "97\n"]
   [(char->integer #\z) => "122\n"]
   [(char->integer #\Z) => "90\n"]
   [(char->integer #\0) => "48\n"]
   [(char->integer #\9) => "57\n"]
   [(char->integer (integer->char 12)) => "12\n"]
   [(integer->char (char->integer #\x)) => "#\\x\n"]
)

(add-tests-with-string-output "integer?"
   [(integer? 0) => "#t\n"]
   [(integer? 1) => "#t\n"]
   [(integer? -1) => "#t\n"]
   [(integer? 37287) => "#t\n"]
   [(integer? -23873) => "#t\n"]
   [(integer? 536870911) => "#t\n"]
   [(integer? -536870912) => "#t\n"]
   [(integer? #t) => "#f\n"]
   [(integer? #f) => "#f\n"]
   [(integer? ()) => "#f\n"]
   [(integer? #\Q) => "#f\n"]
   [(integer? (integer? 12)) => "#f\n"]
   [(integer? (integer? #f)) => "#f\n"]
   [(integer? (integer? #\A)) => "#f\n"]
   [(integer? (char->integer #\r)) => "#t\n"]
   [(integer? (integer->char 12)) => "#f\n"]
)


(add-tests-with-string-output "fxzero?"
   [(zero? 0) => "#t\n"]
   [(zero? 1) => "#f\n"]
   [(zero? -1) => "#f\n"]
)

(add-tests-with-string-output "null?"
   [(null? ()) => "#t\n"]
   [(null? #f) => "#f\n"]
   [(null? #t) => "#f\n"]
   [(null? (null? ())) => "#f\n"]
   [(null? #\a) => "#f\n"]
   [(null? 0) => "#f\n"]
   [(null? -10) => "#f\n"]
   [(null? 10) => "#f\n"]
)

(add-tests-with-string-output "boolean?"
   [(boolean? #t) => "#t\n"]
   [(boolean? #f) => "#t\n"]
   [(boolean? 0) => "#f\n"]
   [(boolean? 1) => "#f\n"]
   [(boolean? -1) => "#f\n"]
   [(boolean? ()) => "#f\n"]
   [(boolean? #\a) => "#f\n"]
   [(boolean? (boolean? 0)) => "#t\n"]
   [(boolean? (integer? (boolean? 0))) => "#t\n"]
)

(add-tests-with-string-output "char?"
   [(char? #\a) => "#t\n"]
   [(char? #\Z) => "#t\n"]
   [(char? #\newline) => "#t\n"]
   [(char? #t) => "#f\n"]
   [(char? #f) => "#f\n"]
   [(char? ()) => "#f\n"]
   [(char? (char? #t)) => "#f\n"]
   [(char? 0) => "#f\n"]
   [(char? 23870) => "#f\n"]
   [(char? -23789) => "#f\n"]
)

(add-tests-with-string-output "not"
  [(not #t) => "#f\n"]
  [(not #f) => "#t\n"]
  [(not 15) => "#f\n"]
  [(not ()) => "#f\n"]
  [(not #\A) => "#f\n"]
  [(not (not #t)) => "#t\n"]
  [(not (not #f)) => "#f\n"]
  [(not (not 15)) => "#t\n"]
  [(not (integer? 15)) => "#f\n"]
  [(not (integer? #f)) => "#t\n"]
)

(add-tests-with-string-output "fxlognot"
 [(lognot 0) => "-1\n"]
 [(lognot -1) => "0\n"]
 [(lognot 1) => "-2\n"]
 [(lognot -2) => "1\n"]
 [(lognot 536870911) => "-536870912\n"]
 [(lognot -536870912) => "536870911\n"]
 [(lognot (lognot 237463)) => "237463\n"]
)

