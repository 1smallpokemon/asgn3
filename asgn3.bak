#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U NumC BinopC PlusC MultC))
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ExprC] [r : ExprC])#:transparent)
(struct MultC ([l : ExprC] [r : ExprC])#:transparent)

;; hash-table for BinopC, converts binary operators to their corresponding
;; racket operation
(define binop-hash
  (hash
   PlusC +
   MultC *))

;; main VVQS parsing function
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? binop-hash s)
                                  (BinopC s (parse l) (parse r))
                                  (error  'parse "expected legal expression, got: ~e" s))]
    [other (error  'parse "expected legal expression, got: ~e" other)]))

(define a1 (PlusC (NumC 1) (NumC 2)))
(define a2 (PlusC (NumC 3) a1))
(define a3 (MultC a1 a2))
(check-equal? (parse '(+ 1 2)) a1)
(check-equal? (parse '(+ 3 (+ 1 2))) a2)
(check-equal? (parse '(* (+ 1 2) (+ 3 (+ 1 2)))) a3)
(check-exn #rx"expected" (lambda () (parse '{+ 4})))

;; interp consumes an abstract syntax tree to produce an answer
(define (interp (ar : ExprC)) : Real
  (match ar
    [(NumC n) n]
    [(BinopC o l r)
     ((hash-ref binop-hash o (error 'interp "invalid binop ~e" o))
                     (interp l) (interp r))]))

(check-equal? (interp a1) 3)
(check-equal? (interp a2) 6)
(check-equal? (interp a3) 18)
(check-equal? (interp (NumC 0)) 0)