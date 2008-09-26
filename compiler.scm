;; loads all our helper functions
(load "helper.scm")

;;;; predicate block ;;;;
;;; these are the predicates to determine 
;;; what a particular form is
(define (immediate? x)
  (or (integer? x) (boolean? x) (char? x) (null? x) (equal? x 'null) (equal? x '(quote ()))))

(define (primcall? x)
  (let ((fun (primcall-func x)))
    (ormap (lambda (x) (equal? fun x))
           '(add1 sub1 integer->char char->integer null? zero? not
                  integer? boolean? + - * / = < > <= >= char=? char<?
                  char>? char<=? char>=?))))

(define (variable? x)
  (symbol? x))

(define (let? x)
  (eq? (car x) 'let))

(define (if? x)
  (eq? (car x) 'if))

;; uses our flags to set the type of the immediate value
(define (immediate-rep x)
  (cond ((integer? x)
         (arithmetic-shift x fixnum-shift))
        ((boolean? x)
         (if x
             (bitwise-ior (arithmetic-shift 1 bool-shift) bool-mask) 
             (bitwise-ior (arithmetic-shift 0 bool-shift) bool-mask)))
        ((char? x)
         (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-mask))
        ((or (null? x) (equal? x 'null) (equal? x '(quote ())))
         hex-null)))

;;;; emit block ;;;;
;;; these spit out asm code for various 
;;; forms. we use printf in them there parts
(define (emit-label label)
  (printf "~a:\n" label))

(define (emit-jump jump-call label)
  (printf "~a ~a\n" jump-call label))

(define (emit-boolean-comp si func immediate env)
  (if immediate
      (printf "cmpl $~a, %eax\n" immediate)
      (printf "cmpl ~a(%esp), %eax\n" si))
  (printf "movl $0, %eax\n")
  (printf "~s %al\n" (func-to-asm func))
  (printf "sall $7, %eax\n")
  (printf "orl $63, %eax\n"))

(define (emit-arithmetic x si env)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env)
    (printf "movl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env)
    (printf "~s ~a(%esp), %eax\n" (func-to-asm func) si)
    (when (equal? func '*)
      (printf "sarl $2, %eax\n"))
    (when (equal? func '/)
      (printf "sall $2, %eax\n"))))

(define (emit-comparison x si env)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env)
    (printf "movl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env)
    (emit-boolean-comp si func #f env)))

(define (emit-primcall x si env)
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
       (emit-boolean-comp si '= hex-null env))
      ((zero?)
       (emit-boolean-comp si '= 0 env))
      ((not)
       (emit-boolean-comp si '= bool-f env))
      ((integer?)
       (printf "andl $~a, %eax\n" fixnum-mask)
       (emit-boolean-comp si '= 0 env))
      ((boolean?)
       (printf "andl $127, %eax\n")
       (emit-boolean-comp si '= 63 env)))

    ;; binary ops
    (cond ((member func '(+ - * /))
           (emit-arithmetic x si env))
          ((member func '(< > = <= =< char<? char>? char=? char<=? char>=?))
           (emit-comparison x si env)))))

(define (emit-let bindings body si env)
  (let f ((b* bindings) (new-env env) (si si))
    (cond
     ((null? b*) (emit-expr body si new-env))
     (else
      (let ((b (car b*)))
        (print-debug '("rhs of let expr: ~a\n") (list (rhs b)))
        (emit-expr (rhs b) si env)
        (printf "movl %eax, ~a(%esp)\n" si)
        (f (cdr b*)
           (extend-env (lhs b) si new-env)
           (- si wordsize)))))))

(define (emit-if test conseq altern si env)
  (let ((L0 (gensym)) (L1 (gensym)))
    (emit-expr test si env)
    (printf "cmpl $~a, %eax\n" bool-t)
    (emit-jump "je" L0)
    (emit-expr altern si env)
    (emit-jump "jmp" L1)
    (emit-label L0)
    (emit-expr conseq si env)
    (emit-label L1)))

(define (emit-expr x si env)
  (cond ((immediate? x)
         (print-debug '("immediate found: ~a\n") (list x))
         (printf "movl $~a, %eax\n" (immediate-rep x)))
        ((variable? x)
         (print-debug '("variable found: ~a\n") (list x))
         (printf "movl ~a(%esp), %eax\n" (lookup x env)))
        ((let? x)
         (print-debug '("let found: ~a\n") (list x))
         (emit-let (bindings x) (body x) si env))
        ((if? x)
         (print-debug '("if found: ~a\n") (list x))
         (emit-if (if-condition x) (true-branch x) (false-branch x) si env))
        ((primcall? x)
         (emit-expr (primcall-operand1 x) si env)
         (emit-primcall x si env))))

;; COMPILE DAT!!!
(define (compile-program x)
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr x -4 '())
  (printf "ret\n"))
