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

(define (primcall-func x)
  (car x))

(define (primcall-operand1 x)
  (cadr x))

(define (emit-boolean-return)
  (printf "movl $0, %eax\n")
  (printf "sete %al\n")
  (printf "sall $7, %eax\n")
  (printf "orl $63, %eax\n"))

(define (immediate? x)
  (or (integer? x) (boolean? x) (char? x) (null? x) (equal? x 'null)))

(define (primcall? x)
  (let ((fun (primcall-func x)))
    (ormap (lambda (x) (equal? fun x))
           '(add1 sub1 integer->char char->integer null? zero? not
                  integer? boolean?))))

(define (immediate-rep x)
  (cond ((integer? x)
         (arithmetic-shift x fixnum-shift))
        ((boolean? x)
         (if x
             (bitwise-ior (arithmetic-shift 1 bool-shift) bool-mask) 
             (bitwise-ior (arithmetic-shift 0 bool-shift) bool-mask)))
        ((char? x)
         (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-mask))
        ((or (null? x) (equal? x 'null))
         hex-null)))

(define (emit-primcall x)
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
       (printf "cmpl $~a, %eax\n" hex-null)
       (emit-boolean-return))
      ((zero?)
       (printf "cmpl $0, %eax\n")
       (emit-boolean-return))
      ((not)
       (printf "cmpl $~a, %eax\n" bool-f)
       (emit-boolean-return))
      ((integer?)
       (printf "andl $~a, %eax\n" fixnum-mask)
       (printf "cmpl $0, %eax\n")
       (emit-boolean-return))
      ((boolean?)
       (printf "andl $127, %eax\n")
       (printf "cmpl $63, %eax\n")
       (emit-boolean-return)))))

(define (emit-expr x)
  (cond ((immediate? x)
         (printf "movl $~a, %eax\n" (immediate-rep x)))
        ((primcall? x)
         (emit-expr (primcall-operand1 x))
         (emit-primcall x))))

(define (compile-program x)
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr x)
  (printf "ret\n"))
