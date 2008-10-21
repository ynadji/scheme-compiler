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

(define (func? x env)
  (symbol? (lookup x env)))

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

(define (letrec? x)
  (eq? (car x) 'letrec))

(define (lambda? x)
  (eq? (car x) 'lambda))

(define (app? x)
  (eq? (car x) 'app))

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

(define (emit-boolean-comp si func immediate env)
  (if immediate
      (printf "\tcmpl $~a, %eax\n" immediate)
      (printf "\tcmpl ~a(%esp), %eax\n" si))
  (printf "\tmovl $0, %eax\n")
  (printf "\t~s %al\n" (func-to-asm func))
  (printf "\tsall $7, %eax\n")
  (printf "\torl $63, %eax\n"))

(define (emit-arithmetic x si env)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env)
    (printf "\tmovl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env)
    (printf "\t~s ~a(%esp), %eax\n" (func-to-asm func) si)
    (when (equal? func '*)
      (printf "\tsarl $2, %eax\n"))
    (when (equal? func '/)
      (printf "\tsall $2, %eax\n"))))

(define (emit-comparison x si env)
  (let ((func (primcall-func x))
        (op1 (primcall-operand1 x))
        (op2 (primcall-operand2 x)))
    (emit-expr op2 si env)
    (printf "\tmovl %eax, ~a(%esp)\n" si)
    (emit-expr op1 (- si wordsize) env)
    (emit-boolean-comp si func #f env)))

(define (emit-primcall x si env)
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
       (emit-boolean-comp si '= hex-null env))
      ((zero?)
       (emit-boolean-comp si '= 0 env))
      ((not)
       (emit-boolean-comp si '= bool-f env))
      ((integer?)
       (printf "\tandl $~a, %eax\n" fixnum-mask)
       (emit-boolean-comp si '= 0 env))
      ((boolean?)
       (printf "\tandl $127, %eax\n")
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
        (printf "\tmovl %eax, ~a(%esp)\n" si)
        (f (cdr b*)
           (extend-env (lhs b) si new-env)
           (- si wordsize)))))))

(define (emit-if test conseq altern si env)
  (let ((L0 (gensym)) (L1 (gensym)))
    (emit-expr test si env)
    (printf "\tcmpl $~a, %eax\n" bool-f)
    (emit-jump "je" L0)
    (emit-expr conseq si env)
    (emit-jump "jmp" L1)
    (emit-label L0)
    (emit-expr altern si env)
    (emit-label L1)))

(define (emit-cons x si env hi)
  (emit-expr (primcall-operand1 x) si env hi)
  (printf "\tmovl %eax, ~a(%esi)\n" hi)
  (emit-expr (primcall-operand2 x) si env)
  (printf "\tmovl %eax, ~a(%esi)\n" (+ wordsize hi))
  (printf "\tmovl %esi, %eax\n")
  (printf "\torl $1, %eax\n")
  (printf "\taddl $8, %esi\n"))

(define (emit-car x si env hi)
  (print-debug '("stack index car: ~a\n") (list si))
  (emit-expr (primcall-operand1 x) si env)
  (printf "\tmovl -1(%eax), %eax\n"))

(define (emit-cdr x si env hi)
  (print-debug '("heap index cdr: ~a\n") (list hi))
  (emit-expr (primcall-operand1 x) si env)
  (printf "\tmovl 3(%eax), %eax\n"))

(define (emit-letrec x)
  (let* ((bindings (bindings x))
         (lvars (map lhs bindings))
         (lambdas (map rhs bindings))
         (labels (unique-labels lvars))
         (env (make-initial-env lvars labels)))
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (body x) env)))

(define (emit-scheme-entry body env)
  (print-debug '("emit-scheme-entry: ~a\n" "body:~a\n" "env:~a\n")
               (list "..." body env))
  (printf ".text\n")
  (printf ".globl _scheme_entry\n")
  (printf "_scheme_entry:\n")
  (emit-expr body -4 env))

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ((fmls (lambda-formals expr))
          (body (lambda-body expr)))
      (let f ((fmls fmls)
              (si (- wordsize))
              (env env))
        (cond ((null? fmls) (emit-expr body si env))
              (else
               (f (cdr fmls)
                  (- si wordsize)
                  (extend-env (car fmls) si env))))))
    (printf "\tret\n\n")))

(define (emit-function-header label)
  (printf ".globl _~a\n" label)
  (printf "_~a:\n" label))

;; this is atrocious, here's how it works
;; if EMIT-APP is given (app ... ) as expr,
;; it calls emit-app for the CDR, or rather, the function call itself.
;; so (app func arg1 arg2) will be called with (emit-app si env (cdr expr))
;; or (func arg1 arg2). This places the code to evaluate the arguments, place
;; them on the stack, and call func. The WHEN clause is to prevent
;; the (app ... ) form from being evaluated. This is horrible and
;; hacky, and you should probably make it less retarded. On the flip side,
;; it works.
(define (emit-app si env expr)
  (define (emit-arguments si args)
    (print-debug '("emit-arguments=> si: ~a\n" "args: ~a\n" "env: ~a\n\n") (list si args env))
    (unless (null? args)
      (cond ((func? (car args) env)
             (print-debug '("we have a func in app!: ~a\n") (list args))
             (emit-app si env args))
            (else
             (emit-expr (car args) si env)
             (printf "\tmovl %eax, ~a(%esp)\n" si)
             (emit-arguments (- si wordsize) (cdr args))))))
  (print-debug '("emit-app=> si: ~a\n" "args: ~a\n" "env: ~a\n\n") (list si expr env))
  (emit-arguments (- si wordsize) (call-args expr))
  (when (not (app? expr))
    (emit-adjust-base (+ si wordsize))
    (emit-call si (lookup (call-target expr) env))
    (emit-adjust-base (- (+ si wordsize)))))

(define (emit-adjust-base si)
  (print-debug '("emit-adjust-base: ~a\n") (list si))
  (printf "\taddl $~a, %esp\n" si))

(define (call-args expr)
  (print-debug '("call-args: ~a\n") (list expr))
  (cdr expr))

(define (call-target expr)
  (print-debug '("call-target: ~a\n") (list expr))
  (car expr))

(define (emit-call si label)
  (print-debug '("emit-call: si => ~a\n") (list si))
  (printf "\tcall _~a\n" label))

(define (emit-expr x si env)
  (print-debug '("emit-expr:~a" "stack index: ~a\n") (list "\n" si))
  (print-debug '("expr: ~a\n" "env: ~a\n") (list x env))
  (cond ((immediate? x)
         (print-debug '("immediate found: ~a\n") (list x))
         (printf "\tmovl $~a, %eax\n" (immediate-rep x)))
        ((and (variable? x)
              (integer? (lookup x env)))
         (print-debug '("variable found: ~a\n") (list x))
         (printf "\tmovl ~a(%esp), %eax\n" (lookup x env)))
        ((cons? x)
         (print-debug '("cons found: ~a\n") (list x))
         (emit-cons x si env))
        ((car? x)
         (print-debug '("car found: ~a\n") (list x))
         (emit-car x si env))
        ((cdr? x)
         (print-debug '("cdr found: ~a\n") (list x))
         (emit-cdr x si env))
        ((let? x)
         (print-debug '("let found: ~a\n") (list x))
         (emit-let (bindings x) (body x) si env))
        ((if? x)
         (print-debug '("if found: ~a\n") (list x))
         (emit-if (if-condition x) (true-branch x) (false-branch x) si env))
        ((letrec? x)
         (print-debug '("letrec found: ~a\n") (list x))
         (emit-letrec x))
        ((app? x)
         (print-debug '("app found: ~a\n") (list x))
         (emit-app si env x))
        ((primcall? x)
         (print-debug '("primcall found: ~a\n") (list x))
         (emit-expr (primcall-operand1 x) si env)
         (emit-primcall x si env))
        (else
         (print-debug '("function call found: ~a\n") (list x))
         (emit-app si env x))))

;; COMPILE DAT!!!
(define (compile-program x)
  (emit-expr x -4 '())
  (printf "\tret\n"))