 ;; immediate constant defines
(define fixnum-shift 2)
(define fixnum-mask #x3)

(define char-shift 8)
(define char-mask #xf)

(define hex-null #x2f)

(define bool-mask #x3f)
(define bool-shift 7)

(define bool-t #xbf)
(define bool-f #x3f)

(define wordsize 4)

;; debug statements, set this to true to print 'em
;(define debug #t)
; debug is now passed in by the shell script so i can more easily
; turn it on and off

(define (print-debug msg args)
  (if debug
      (map (lambda (x y)
             (printf "DEBUG: ")
             (printf x y)) msg args)))

;; local variable handling
(define (extend-env var-name si env)
  (cons (cons var-name si) env))

(define (make-initial-env lvars labels)
  (map (lambda (x y) (cons x y)) lvars labels))

(define (lookup x env)
  (let ((res (assoc x env)))
    (if res
        (cdr res)
        res)))

;; function lookups
(define (func-to-asm func)
  (cadr (assoc func
               '((+ addl) (- subl) (* imull) (/ idivl)
                 (< setl) (char<? setl)
                 (> setg) (char>? setg)
                 (= sete) (char=? sete)
                 (>= setge) (char>=? setge)
                 (<= setle) (char<=? setle)))))

;; label stuff
(define (unique-labels vars)
  (map gensym vars))

;; accessor functions
(define (primcall-func x)
  (car x))

(define (primcall-operand1 x)
  (cadr x))

(define (primcall-operand2 x)
  (caddr x))

(define (bindings x)
  (cadr x))

(define (body x)
  (caddr x))

(define (rhs x)
  (cadr x))

(define (lhs x)
  (car x))

(define (if-condition x)
  (cadr x))

(define (true-branch x)
  (caddr x))

(define (false-branch x)
  (cadddr x))

(define (lambda-formals x)
  (cadr x))

(define (lambda-body x)
  (caddr x))