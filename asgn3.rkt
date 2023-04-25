#lang typed/racket

(require typed/rackunit)
(struct IdC ([id : Symbol]) #:transparent)
(define-type FundefC (U FunC))
(struct FunC ([name : Symbol] [arg : Symbol] [body : ExprC])#:transparent)

(define-type ExprC (U NumC BinopC leq0? IdC FunAppC))
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FunAppC ([fun : Symbol] [arg : ExprC]) #:transparent)

;; hash-table for BinopC, converts binary operators to their corresponding
;; racket operation
(define ops
  (hash
   '+ +
   '* *
   '- -
   '/ /))

(define (ValidSymbol? [sym : Symbol]) : Boolean
  (cond
    [(hash-has-key? ops sym) #f]
    [else (match sym
            ['def #f]
            ['leq0? #f]
            ['else #f]
            ['then #f]
            ['= #f])]))

(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (list (? symbol? (? ValidSymbol? id))
                      (? symbol? (? ValidSymbol? arg))) exp)
     (FunC id arg (parse exp))]))

(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (map parse-fundef s))

(define (lookup-fun (name : Symbol) (funs : (Listof FundefC))) : FundefC
  (cond [(empty? funs) (error 'interp "VVQS: function not found ~a" name)]
        [else
         (define f (car funs))
         (cond [(FunC? f) (if (symbol=? name (FunC-name f)) f (lookup-fun name (cdr funs)))]
               [else (lookup-fun name (cdr funs))])]))

(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (lookup-fun 'main funs))
  (define init (NumC 0))
  (interp (FunC-body main) funs))


;; main VVQS parsing function
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? ops s)
                                  (BinopC s (parse l) (parse r))
                                  (error 'parse "VVQS: illegal operator ~e" s))]
    [(list 'leq0? test then else)
     (leq0? (parse test) (parse then) (parse else))]
    [(? symbol? id) (IdC id)]
    [(list (? symbol? f) arg)
     (FunAppC f (parse arg))]))

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
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC o l r)
     ((hash-ref ops o) (interp l funs) (interp r funs))]
    [(leq0? test then else) (if (<= (interp test funs) 0)
                                 (interp then funs)
                                 (interp else funs))]
    [(IdC id) (error 'interp "VVQS: unbound identifier ~a" id)]
    [(FunAppC fun arg)
     (define fun-def (lookup-fun fun funs))
     (define arg-val (interp arg funs))
     (define substituted-body (subst (FunC-arg fun-def) (NumC arg-val) (FunC-body fun-def)))
     (interp substituted-body funs)]))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

(define (subst (x : Symbol) (v : ExprC) (e : ExprC)) : ExprC
  (match e
    [(NumC _) e]
    [(IdC id) (if (symbol=? x id) v e)]
    [(BinopC o l r) (BinopC o (subst x v l) (subst x v r))]
    [(leq0? test then else) (leq0? (subst x v test) (subst x v then) (subst x v else))]
    [(FunAppC fun arg) (FunAppC fun (subst x v arg))]))
  
(check-equal? (interp a1) 3)
(check-equal? (interp a2) 6)
(check-equal? (interp a3) 18)
(check-equal? (interp sub) 1)
(check-equal? (interp div) 2)
(check-equal? (interp (NumC 0)) 0)
(check-exn #rx"zero" (λ () (interp (BinopC '/ (NumC 5) (NumC 0)))))
(check-equal? (interp leq0-1) 6)
(check-equal? (interp leq0-2) 3)