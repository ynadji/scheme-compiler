;; immediate constant defines
(define fixnum-shift 2)
(define fixnum-mask #x3)

(define char-shift 8)
(define char-mask #xf)

(define hex-null #x2f)

(define bool-t #x9f)
(define bool-f #x1f)

(define wordsize 4)

(define (immediate? x)
  (or (integer? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond ((integer? x)
         (arithmetic-shift x fixnum-shift))
        ((boolean? x)
         (if x
             bool-t
             bool-f))
        ((char? x)
         (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-mask))
        ((null? x)
         hex-null)))

(define (emit-expr x)
  (cond ((immediate? x)
         (printf "movl $~a, %eax\n" (immediate-rep x)))))

(define (compile-program x)
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr x)
  (printf "ret\n"))