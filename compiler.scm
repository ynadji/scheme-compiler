;; loads all our helper functions
(load "accessor-and-constants.scm")

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

(define (cons? x)
  (eq? (car x) 'cons))

(define (car? x)
  (eq? (car x) 'car))

(define (cdr? x)
  (eq? (car x) 'cdr))

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
  (printf "\t~a ~a\n" jump-call label))

(define (emit-boolean-comp si func immediate env hi)
  (if immediate
      (printf "\tcmpl $~a, %eax\n" immediate)
      (printf "\tcmpl ~a(%esp), %eax\n" si))
  (printf "\tmovl $0, %eax\n")
  (printf "\t~s %al\n" (func-to-asm func))
  (printf "\tsall $7, %eax\n")
  (printf "\torl $63, %eax\n"))

(define (emit-arithmetic x si env hi)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env hi)
    (printf "\tmovl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env hi)
    (printf "\t~s ~a(%esp), %eax\n" (func-to-asm func) si)
    (when (equal? func '*)
      (printf "\tsarl $2, %eax\n"))
    (when (equal? func '/)
      (printf "\tsall $2, %eax\n"))))

(define (emit-comparison x si env hi)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env hi)
    (printf "\tmovl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env hi)
    (emit-boolean-comp si func #f env hi)))

(define (emit-primcall x si env hi)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x)))
    (case func
      ((add1)
       (printf "\taddl $~a, %eax\n" (immediate-rep 1)))
      ((sub1)
       (printf "\tsubl $~a, %eax\n" (immediate-rep 1)))
      ((integer->char)
       (printf "\tsall $6, %eax\n")
       (printf "\torl $15, %eax\n"))
      ((char->integer)
       (printf "\tsarl $6, %eax\n"))
      ((null?)
       (emit-boolean-comp si '= hex-null env hi))
      ((zero?)
       (emit-boolean-comp si '= 0 env hi))
      ((not)
       (emit-boolean-comp si '= bool-f env hi))
      ((integer?)
       (printf "\tandl $~a, %eax\n" fixnum-mask)
       (emit-boolean-comp si '= 0 env hi))
      ((boolean?)
       (printf "\tandl $127, %eax\n")
       (emit-boolean-comp si '= 63 env hi)))

    ;; binary ops
    (cond ((member func '(+ - * /))
           (emit-arithmetic x si env hi))
          ((member func '(< > = <= =< char<? char>? char=? char<=? char>=?))
           (emit-comparison x si env hi)))))

(define (emit-let bindings body si env hi)
  (let f ((b* bindings) (new-env env) (si si))
    (cond
     ((null? b*) (emit-expr body si new-env hi))
     (else
      (let ((b (car b*)))
        (print-debug '("rhs of let expr: ~a\n") (list (rhs b)))
        (emit-expr (rhs b) si env hi)
        (printf "\tmovl %eax, ~a(%esp)\n" si)
        (f (cdr b*)
           (extend-env (lhs b) si new-env)
           (- si wordsize)))))))

(define (emit-if test conseq altern si env hi)
  (let ((L0 (gensym)) (L1 (gensym)))
    (emit-expr test si env hi)
    (printf "\tcmpl $~a, %eax\n" bool-f)
    (emit-jump "je" L0)
    (emit-expr conseq si env hi)
    (emit-jump "jmp" L1)
    (emit-label L0)
    (emit-expr altern si env hi)
    (emit-label L1)))

(define (emit-cons x si env hi)
  (emit-expr (primcall-operand1 x) si env hi)
  (printf "\tmovl %eax, ~a(%esi)\n" hi)
  (emit-expr (primcall-operand2 x) si env (+ wordsize hi))
  (printf "\tmovl %eax, ~a(%esi)\n" (+ wordsize hi))
  (printf "\tmovl %esi, %eax\n")
  (printf "\torl $1, %eax\n")
  (printf "\taddl $8, %esi\n"))

(define (emit-car x si env hi)
  (print-debug '("stack index car: ~a\n") (list si))
  (emit-expr (primcall-operand1 x) si env hi)
  (printf "\tmovl -1(%eax), %eax\n"))

(define (emit-cdr x si env hi)
  (print-debug '("heap index cdr: ~a\n") (list hi))
  (emit-expr (primcall-operand1 x) si env hi)
  (printf "\tmovl 3(%eax), %eax\n"))

(define (emit-expr x si env hi)
  (cond ((immediate? x)
         (print-debug '("immediate found: ~a\n") (list x))
         (printf "\tmovl $~a, %eax\n" (immediate-rep x)))
        ((variable? x)
         (print-debug '("variable found: ~a\n") (list x))
         (printf "\tmovl ~a(%esp), %eax\n" (lookup x env)))
        ((cons? x)
         (print-debug '("cons found: ~a\n") (list x))
         (emit-cons x si env hi))
        ((car? x)
         (print-debug '("car found: ~a\n") (list x))
         (emit-car x si env hi))
        ((cdr? x)
         (print-debug '("cdr found: ~a\n") (list x))
         (emit-cdr x si env hi))
        ((let? x)
         (print-debug '("let found: ~a\n") (list x))
         (emit-let (bindings x) (body x) si env hi))
        ((if? x)
         (print-debug '("if found: ~a\n") (list x))
         (emit-if (if-condition x) (true-branch x) (false-branch x) si env hi))
        ((primcall? x)
         (emit-expr (primcall-operand1 x) si env hi)
         (emit-primcall x si env hi))))

;; COMPILE DAT!!!
(define (compile-program x)
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr x -4 '() 0)
  (printf "\tret\n"))
