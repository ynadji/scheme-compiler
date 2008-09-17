;; Most useful keybindings
; C-c C-e -- compile defn
; C-c M-e -- compile defn/switch to compiler buffer
; M-p/M-n -- previous/next in buffer history
; C-x C-e -- send previous sexp

(define (square x)
  (* x x))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))