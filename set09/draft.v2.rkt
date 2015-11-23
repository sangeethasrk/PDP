;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 40); hours

;; ---Data Definitions---

; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name
; TEMPLATE:
; program-fn: Program -> ???
;(define (program-fn p)
;  (cond
;    [(empty? p) empty]
;    [(def? p) ...(def-name p)...(def-params p)...(def-body p)...]
;    [else (expr-fn p)...]))


; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define-struct def (name params body))
;TEMPLATE:
;def-fn: Def -> ???
;(define (def-fn d)
;  (... (def-name d) ... (def-params d) ... (def-body d) ...))

; A Var is a Symbol, representing PDPLang variable.
; A FnName is a Var, representing a function name.


; An Expr is one of:
; - Number
; - Boolean
; - Var
; - ErrString
; - Lambda
; - (make-arith ArithOp 2ListOf<Expr>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<Expr>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<Expr>)     ; a comparison expression
; - (make-if-exp Expr Expr Expr) ; an if conditional
; - (make-call Expr ListOf<Expr>) ; a function call
; Represents a PDPLang expression.
; TEMPLATE:
; expr-fn: Expr -> ???
;(define (expr-fn e)
;  (cond
;    [(number? e) ...]
;    [(boolean? e) ......]
;    [(var? e)...]
;    [(string? e) ...]
;    [(lambda? e) ...]
;    [(arith? e) ...(arith-op e)...(arith-args e)...]
;    [(bool? e) ...(bool-op e)...(bool-args e)...]
;    [(com? e) ...(cmp-op e)...(cmp-args e)...]
;    [(if-exp? e) ...(if-exp-test e)...
;                 (if-exp-branch1 e)...(if-exp-branch2 e)...]
;    [(call? e) ...(call-fn e)...(call-args e)...]))

(define-struct arith (op args))
(define-struct bool (op args))
(define-struct cmp (op args))
(define-struct if-exp (test branch1 branch2))
(define-struct call (fn args))



; An ErrString is a String, representing a PDPLang error message.

; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define-struct lam (params body))

; A Param is a Var, representing a function parameter.

; An ArithOp is one of:
; - '+
; - '-
; - '*
; - '/
; Represents an arithmetic operation in PDPLang

; A BoolOp is one of:
; - 'and
; - 'or
; Represents a boolean operation in PDPLang

; A CmpOp is one of:
; - '=
; - '<
; - '>
; Represents a comparison operation in PDPLang

; A Result is a:
; - Number
; - Boolean
; - ErrString
; - Lambda

; An Operator is one of:
;- +
;- -
;- *
;- /
;- =
;- <
;- >

; An ExprNoVar is one of:
; - Number
; - Boolean
; - StaticDist
; - ErrString
; - LamNoVar
; - (make-arith ArithOp 2ListOf<ExprNoVar>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<ExprNoVar>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<ExprNoVar>)     ; a comparison expression
; - (make-if-exp ExprNoVar ExprNoVar ExprNoVar) ; an if conditional
; - (make-call ExprNoVar ListOf<ExprNoVar>) ; a function call
; Represents an Expr without explicit variables.
 
; A StaticDist is a (list Depth Index)
; Represents a variable reference
; where depth is number of additional lambdas between this var ref and the
; lambda for which this variable is a parameter,
; and index is the (0-based) position of this variable in that lambda's
; parameter list.
 
; A Depth is a Natural
; An Index is a Natural
 
; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body))

(define ERRSTR "err: wrong kind of arguments")
(define DIV-BY-ZERO "err: /: division by zero")
(define ERR-IF "err: if: question result is neither true nor false")
(define UNDEF-VAR "err: undefined var")
(define ZERO 0)


(define (var? e)
  (symbol? e))

; lambda? : Expr -> Boolean
; Returns true if e is a PDPLang lambda expression.
(define (lambda? e)
  (lam? e))
; errstr? : Expr -> Boolean
; Returns true if e is a PDPLang ErrString expression.
(define (errstr? e)
  (string? e))


; eval : Program -> ListOf<Result>
; Evaluates a PDPLang program to a list of Results.
; Specifically, evaluates the Exprs in p, in the context of the given Defs.
; WHERE: A function may be called before it is defined.
; WHERE: The produced results are in the same relative order as their 
; originating Exprs in p.
(define (eval p)
  (local
    ((define (eval-main p deflst)
       (cond
         [(empty? p) empty]
         [(def? (first p)) (eval-main (rest p) deflst)]
         [else (cons (process-expr (first p) deflst)
                     (eval-main (rest p) deflst))])))
    (eval-main p (get-defs p))))       

(define (get-defs p)
  (filter def? p))

; process-expr: Expr ListOf<Def> -> Result
(define (process-expr e d)
  (cond
    [(number? e) e]
    [(boolean? e) e]
    [(var? e) (make-lambda e d)]
    [(errstr? e) e]
    [(lambda? e) e]
    [(arith? e) (handle-algebra (arith-op e) (arith-args e) d
                                number? perform-algebraic-opn)]
    [(bool? e) (handle-algebra (bool-op e) (bool-args e) d
                               boolean? perform-bool-opn)]                   
    [(cmp? e) (handle-algebra (cmp-op e) (cmp-args e) d
                              number? perform-algebraic-opn)]              
    [(if-exp? e) (handle-if (if-exp-test e) (if-exp-branch1 e)
                            (if-exp-branch2 e) d)]
    [(call? e) (handle-call (call-fn e) (call-args e) d)]))
  

(define (make-lambda var defs)
  (local
    ((define d (get-fn-def var defs)))
    (if (def? d)
        (make-lam (def-params d) (def-body d))
        ERRSTR)))

(define (get-fn-def fn defs)
  (local
    ((define (fn-def fn d)
       (filter (λ(l) (symbol=? fn (def-name l))) d)))
    (if (empty? (fn-def fn defs))
        #f
        (first (fn-def fn defs)))))

(define (handle-algebra op args defs type? perform-opn)
  (local
    ((define (process-args arg)
       (map (λ(l) (process-expr l defs)) arg)))
    (if (andmap type? (process-args args))
        (perform-opn op (process-args args))
        ERRSTR)))


(define (perform-algebraic-opn op args)
  (if (division-by-zero? op args)
      DIV-BY-ZERO
      (apply (dequote op) args)))


(define (division-by-zero? op args)
  (if (symbol=? op '/)
      (member? ZERO args)
      #f))

(define (perform-bool-opn op args)
  (if (symbol=? 'and op)
      (foldr and-and #t args)
      (foldr or-or  #f args)))

(define (handle-if test case1 case2 defs)
  (local
    ((define (if-result tst)
       (process-expr tst defs)))
    (if (boolean? (if-result test))
        (perform-if-else-opn (if-result test) case1 case2 defs)
        ERR-IF)))

(define (perform-if-else-opn test case1 case2 defs)
  (if test
      (process-expr case1 defs)
      (process-expr case2 defs)))


(define (handle-call fn args defs)
  (local
    ((define may-be-lam (process-expr fn defs)))
  (if (lambda? may-be-lam)
      (process-expr
       (recursive-subst args (lam-params may-be-lam)
                        (lam-body may-be-lam)) defs)
      ERRSTR)))

(define (recursive-subst lst1 lst2 expr)
  (cond
    [(empty? lst1) expr]
    [else (if (equal? (length lst1) (length lst2))
              (subst (first lst1) (first lst2)
                     (recursive-subst (rest lst1) (rest lst2) expr))
              UNDEF-VAR)]))  

(define (and-and x y)
  (and x y))

(define (or-or x y)
  (or x y))

; subst : Result Var Expr -> Expr
; Replaces references to x in e with r.
; Does not replace x with r if x occurs in the body of a lambda
; that shadows x.
; WHERE: r has no unbound variables
(define (subst r x e)
  (cond
    [(number? e) e]
    [(boolean? e) e]
    [(var? e) (if (symbol=? e x)
                  r
                  e)]
    [(errstr? e) e]
    [(lambda? e) (if (member? x (lam-params e))
                     e
                     (make-lam (lam-params e) (subst r x (lam-body e))))]
    [(arith? e) (make-arith (arith-op e) (subst-on-list r x (arith-args e)))]
    [(bool? e) e]
    [(cmp? e) (make-cmp (cmp-op e) (subst-on-list r x (cmp-args e)))]
    [(if-exp? e) (make-if-exp (subst r x (if-exp-test e))
                              (subst r x (if-exp-branch1 e))
                              (subst r x (if-exp-branch2 e)))]
    [(call? e) (make-call (subst r x (call-fn e))
                          (subst-on-list r x (call-args e)))]))

; subst-on-list: Result var ListOf<Expr> -> ListOf<Expr>
; applies the function subst on the given list of exprs
; and returns the resulting exprs as a list
; EXAMPLES:
(begin-for-test
  (check-equal? (subst-on-list 5 'x (list (make-arith '+ '(x y)) 4))
                (list (make-arith '+ '(5 y)) 4)  
                "true"))
; STRATEGY: Function Composition
(define (subst-on-list r x lste)
  (map (λ(e) (subst r x e)) lste))
; Test-suite:
(begin-for-test
  (check-equal? (subst-on-list 10 'y (list (make-arith '+ '(x y))))
                (list (make-arith '+ '(x 10)))
                "true"))


; dequote : ArithOp or CmpOp -> Operator
(define (dequote op)
  (cond
    [(symbol=? '+ op) +]
    [(symbol=? '- op) -]
    [(symbol=? '* op) *]
    [(symbol=? '/ op) /]
    [(symbol=? '= op) =]
    [(symbol=? '< op) <]
    [(symbol=? '> op) >]))


; expr->expr/no-var : Expr -> ExprNoVar
; Replaces Var in e with StaticDist.
; WHERE: there are no unbound variables in e.
(define (expr->expr/no-var e)
  (local
    (; : Expr -> expr/no-var
     ; converts expr to expr wit no var
     ; accumulator acc contains the lambda parameters
     ; that are seen in this iteration
     ; STRATEGY: Data decomposition on e: expr     
     (define (expr->expr/no-var/a e acc)
       (cond
         [(number? e) e]
         [(boolean? e) e]
         [(var? e) (find-static-dist e acc)]
         [(errstr? e) e]
         [(lambda? e) (make-lam/no-var
                       (expr->expr/no-var/a (lam-body e)
                                            (add-at-last (lam-params e) acc)))]
         [(arith? e) (make-arith
                      (arith-op e) (map (λ(l) (expr->expr/no-var/a l acc))
                                    (arith-args e)))]
         [(bool? e) e]
         [(cmp? e) (make-cmp
                    (cmp-op e) (map (λ(l) (expr->expr/no-var/a l acc))
                                (cmp-args e)))]  
         [(if-exp? e) (make-if-exp (expr->expr/no-var/a (if-exp-test e) acc)
                                   (expr->expr/no-var/a (if-exp-branch1 e) acc)
                                   (expr->expr/no-var/a (if-exp-branch2 e)
                                                        acc))]
         [(call? e) (make-call (expr->expr/no-var/a (call-fn e) acc)
                               (map (λ(l) (expr->expr/no-var/a l acc)) 
                                (call-args e)))])))
    (expr->expr/no-var/a e empty)))
     

; find-static-dist: ListOf<var> ListOf<ListOf<var>>
; checks if the given arg is present in lam-args and 
; returns the depth and index of that var.
; STRATEGY: Data Decomposition on lam-args: ListOf<ListOf<var>>
(define (find-static-dist arg lam-args)
  (cond
    [(empty? lam-args) empty]
    [else
     (if (member? arg (first lam-args))
         (list (length (rest lam-args))
               (index-of arg (first lam-args)))
         (find-static-dist arg (rest lam-args)))]))

(define (index-of item lst)
  (- (length lst)
     (length (memv item lst))))

; expr=? : Expr Expr -> Boolean
; Returns true if e1 and e2 are structurally equivalent, up to some
; renaming of variable names.
(define (expr=? e1 e2)
  (equal? (expr->expr/no-var e1)
       (expr->expr/no-var e2)))       

; add-at-last : Any ListOf<Any> -> ListOf<Any>
; Adds the item at the last of the given list
; EXAMPLES: 
(begin-for-test
  (check-equal? (add-at-last 3 (list 0 1 2))
                (list 0 1 2 3)
                "Function failed"))
; STRATEGY: Function Composition
(define (add-at-last item lst)
  (append
   lst
   (list item)))