#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U NumC BinopC leq0?))
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;; hash-table for BinopC, converts binary operators to their corresponding
;; racket operation
(define ops
  (hash
   '+ +
   '* *
   '- -
   '/ /))

;; main VVQS parsing function
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? ops s)
                                  (BinopC s (parse l) (parse r))
                                  (error 'parse "VVQS: illegal operator ~e" s))]
    [(list 'leq0? test then else)
     (leq0? (parse test) (parse then) (parse else))]
    [other (error 'parse "VVQS: illegal expression: ~e" other)]))

(define a1 (BinopC '+ (NumC 1) (NumC 2)))
(define a2 (BinopC '+ (NumC 3) a1))
(define a3 (BinopC '* a1 a2))
(define sub (BinopC '- (NumC 3) (NumC 2)))
(define div (BinopC '/ (NumC 4) (NumC 2)))
(define leq0-1 (leq0? (NumC 1) a1 a2))
(define leq0-2 (leq0? (NumC -1) a1 a2))
(check-equal? (parse '(+ 1 2)) a1)
(check-equal? (parse '(+ 3 (+ 1 2))) a2)
(check-equal? (parse '(* (+ 1 2) (+ 3 (+ 1 2)))) a3)
(check-equal? (parse '(- 3 2)) sub)
(check-equal? (parse '(/ 4 2)) div)
(check-exn #rx"expression" (lambda () (parse '{+ 4})))
(check-exn #rx"operator" (lambda () (parse '{& 4 5})))
(check-equal? (parse '(leq0? 1 (+ 1 2) (+ 3 (+ 1 2)))) leq0-1)

;; interp consumes an abstract syntax tree to produce an answer
(define (interp (ar : ExprC)) : Real
  (match ar
    [(NumC n) n]
    [(BinopC '/ l (NumC 0)) (error 'interp "VVQS: divide by zero")]
    [(BinopC o l r)
     ((hash-ref ops o) (interp l) (interp r))]
    [(leq0? test then else) (if (<= (interp test) 0)
                                (interp then)
                                (interp else))]))

(check-equal? (interp a1) 3)
(check-equal? (interp a2) 6)
(check-equal? (interp a3) 18)
(check-equal? (interp sub) 1)
(check-equal? (interp div) 2)
(check-equal? (interp (NumC 0)) 0)
(check-exn #rx"zero" (λ () (interp (BinopC '/ (NumC 5) (NumC 0)))))
(check-equal? (interp leq0-1) 6)
(check-equal? (interp leq0-2) 3)