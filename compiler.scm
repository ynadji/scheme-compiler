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

(define (func-to-asm func)
  (cadr (assoc func
               '((+ addl) (- subl) (* imull) (/ idivl)
                 (< setl) (char<? setl)
                 (> setg) (char>? setg)
                 (= sete) (char=? sete)
                 (>= setge) (char>=? setge)
                 (<= setle) (char<=? setle)))))

(define (primcall-func x)
  (car x))

(define (primcall-operand1 x)
  (cadr x))

(define (primcall-operand2 x)
  (caddr x))

(define (emit-boolean-comp si func immediate)
  (if immediate
      (printf "cmpl $~a, %eax\n" immediate)
      (printf "cmpl ~a(%esp), %eax\n" si))
  (printf "movl $0, %eax\n")
  (printf "~s %al\n" (func-to-asm func))
  (printf "sall $7, %eax\n")
  (printf "orl $63, %eax\n"))

(define (immediate? x)
  (or (integer? x) (boolean? x) (char? x) (null? x) (equal? x 'null) (equal? x '(quote ()))))

(define (primcall? x)
  (let ((fun (primcall-func x)))
    (ormap (lambda (x) (equal? fun x))
           '(add1 sub1 integer->char char->integer null? zero? not
                  integer? boolean? + - * / = < > <= >= char=? char<?
                  char>? char<=? char>=?))))

(define (immediate-rep x)
  (cond ((integer? x)
         (arithmetic-shift x fixnum-shift))
        ((boolean? x)
         (if x
             (bitwise-ior (arithmetic-shift 1 bool-shift) bool-mask) 
             (bitwise-ior (arithmetic-shift 0 bool-shift) bool-mask)))
        ((char? x)
         (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-mask))
        ((or (null? x) (equal? x 'null) (equal? x'(quote ())))
         hex-null)))

(define (emit-arithmetic x si)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si)
    (printf "movl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize))
    (printf "~s ~a(%esp), %eax\n" (func-to-asm func) si)
    (when (equal? func '*)
      (printf "sarl $2, %eax\n"))
    (when (equal? func '/)
      (printf "sall $2, %eax\n"))))

(define (emit-comparison x si)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si)
    (printf "movl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize))
    (emit-boolean-comp si func #f)))

(define (emit-primcall x si)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x)))
    (case func
      ((add1)
       (printf "addl $~a, %eax\n" (immediate-rep 1)))
      ((sub1)
       (printf "subl $~a, %eax\n" (immediate-rep 1)))
      ((integer->char)
       (printf "sall $6, %eax\n")
       (printf "orl $15, %eax\n"))
      ((char->integer)
       (printf "sarl $6, %eax\n"))
      ((null?)
       (emit-boolean-comp si '= hex-null))
      ((zero?)
       (emit-boolean-comp si '= 0))
      ((not)
       (emit-boolean-comp si '= bool-f))
      ((integer?)
       (printf "andl $~a, %eax\n" fixnum-mask)
       (emit-boolean-comp si '= 0))
      ((boolean?)
       (printf "andl $127, %eax\n")
       (emit-boolean-comp si '= 63)))

    ;; binary ops
    (cond ((member func '(+ - * /))
           (emit-arithmetic x si))
          ((member func '(< > = <= =< char<? char>? char=? char<=? char>=?))
           (emit-comparison x si)))))

(define (emit-expr x si)
  (cond ((immediate? x)
         (printf "movl $~a, %eax\n" (immediate-rep x)))
        ((primcall? x)
         (emit-expr (primcall-operand1 x) si)
         (emit-primcall x si))))

(define (compile-program x)
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr x -4)
  (printf "ret\n"))