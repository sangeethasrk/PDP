;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eval) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 40); hours

(provide eval)
(provide lambda?)
(provide errstr?)
(provide subst)
(provide expr->expr/no-var)
(provide expr=?)

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
;  (... (def-name d) ... (lox-fn (def-params d)) ...
;                    ...(expr-fn (def-body d)...) ...))

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
;    [(number? e) ...(num-fn e)...]
;    [(boolean? e) ...(boolean-fn e)...]
;    [(var? e)...(var-fn e)...]
;    [(errstr? e) ...(str-fn e)...]
;    [(lambda? e) ...(ulox-fn lam-params e)...(expr-fn (lam-body e)...]
;    [(arith? e) ...(op-fn (arith-op e))...(2lox-fn (arith-args e))...]
;    [(bool? e) ...((op-fn (bool-op e))...(2lox-fn (bool-args e))...]
;    [(cmp? e) ...(op-fn (cmp-op e)...(2lox-fn (cmp-args e))...]
;    [(if-exp? e) ...(expr-fn (if-exp-test e))...
;                (expr-fn (if-exp-branch1 e))...(expr-fn (if-exp-branch2 e))...]
;    [(call? e) ...(expr-fn (call-fn e))...(...(call-args e)...)]))

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

; An OperatorFunction is one of:
;- +
;- -
;- *
;- /
;- =
;- <
;- >
;- and-and
;- or-or

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

;---DATA EXAMPLES
(define MK-ADD-DEF
  (make-def
   'mk-add '(n)
   (make-lam '(m) (make-arith '+ '(n m)))))
(define ADD5-DEF
  (make-def
   'add5 '(x)
   (make-call (make-call 'mk-add '(5)) '(x))))
; add5-or-6 : adds 5 to y if y it's positive, else adds 6
(define ADD5-OR-6-DEF
  (make-def
   'add5-or-6 '(y)
   (make-if-exp (make-cmp '> '(y 0))
                (make-call 'add5 '(y))
                (make-call (make-call 'mk-add '(6)) '(y)))))

(define ERRSTR "err: wrong kind of arguments");errstr
(define DIV-BY-ZERO "err: /: division by zero");errstr
(define ERR-IF "err: if: question result is neither true nor false"); errstr
(define UNDEF-VAR "err: undefined var");err string for undefined var
(define ZERO 0);zero

; var? : Expr -> Boolean
; Returns true if the passed expression is a PDPLang variable
; Examples:
(begin-for-test
  (check-equal? (var? '+) #true "true"))
; Strategy : Function Composition
(define (var? e)
  (symbol? e))
; Test-suite:
(begin-for-test
  (check-equal? (var? "z") #false "true"))

; lambda? : Expr -> Boolean
; Returns true if e is a PDPLang lambda expression.
; Examples:
(begin-for-test
  (check-equal? (lambda? (make-lam '(x y) (make-arith '+ '( x y) )))
                #true "true"))
; Strategy : Function Composition
(define (lambda? e)
  (lam? e))
; Test-suite
(begin-for-test
  (check-equal? (lambda? 'z) #false "true"))

; errstr? : Expr -> Boolean
; Returns true if e is a PDPLang ErrString expression.
; Examples:
(begin-for-test
  (check-equal? (errstr? "error string") #true "true"))
; Strategy : Function Composition
(define (errstr? e)
  (string? e))
; test-suite
(begin-for-test
  (check-equal? (errstr? '+) #false "true"))


; eval : Program -> ListOf<Result>
; Evaluates a PDPLang program to a list of Results.
; Specifically, evaluates the Exprs in p, in the context of the given Defs.
; WHERE: A function may be called before it is defined.
; WHERE: The produced results are in the same relative order as their 
; originating Exprs in p.
; Examples:
(begin-for-test
  (check-equal? (eval '()) '() "true")
  (check-equal? (eval (list MK-ADD-DEF
                            (make-call 'add5 '(4))))
                (list "err: wrong kind of arguments")
                "true"))
; Strategy : Function Composition
(define (eval p)
  (local
    (; eval-main: Program ListOf<Def> -> ListOf<Result>
     ; evaluates the exprs in the program and generates
     ; equivalent list of results.
     ; STRATEGY: Data Decomposition on p : Program
     (define (eval-main p deflst)
       (cond
         [(empty? p) empty]
         [(def? (first p)) (eval-main (rest p) deflst)]
         [else (cons (process-expr (first p) deflst)
                     (eval-main (rest p) deflst))])))
    (eval-main p (get-defs p))))
; Test Suite
(begin-for-test
  (check-equal?
   (eval
    (list
     MK-ADD-DEF
     ADD5-DEF
     ADD5-OR-6-DEF
     (make-call 'add5 '(10))
     (make-call 'add5-or-6 '(200))
     (make-call 'add5-or-6 '(-100))))
   (list 15 205 -94)
   "true"))

; get-defs : Program -> ListOf<Def>
; Filters list of definitions
; from the input program
; Examples:
(begin-for-test
  (check-equal? (get-defs (list MK-ADD-DEF (make-call 'add5 '(4))))
                (list (make-def 'mk-add (list 'n) 
                                (make-lam (list 'm) 
                                          (make-arith '+ (list 'n 'm)))))
                "true"))
; Strategy : Function Composition
(define (get-defs p)
  (filter def? p))
; Test Suite:
(begin-for-test
  (check-equal? (get-defs (list (make-call 'add5-or-6 '(-100))))
                empty
                "true"))

; process-expr: Expr ListOf<Def> -> Result
; Processes the recieved expression and evaluates them
; in context of given Defs
; Examples:
(begin-for-test
  (check-equal? (process-expr 
                 (make-call 'add5 (list 'y)) (list MK-ADD-DEF ADD5-DEF))
                "err: wrong kind of arguments"
                "true")
  (check-equal? (process-expr 
                 (make-call 'add5 (list 4)) (list MK-ADD-DEF ADD5-DEF))
                9
                "true"))
; Strategy : Data Decomposition on e : Expr
(define (process-expr e d)
  (cond
    [(number? e) e]
    [(boolean? e) e]
    [(var? e) (make-lambda e d)]
    [(errstr? e) e]
    [(lambda? e) e]
    [(arith? e) (handle-arith (dequote (arith-op e))
                              (reverse (arith-args e)) d)]
    [(bool? e) (handle-bool (dequote (bool-op e)) (bool-args e) d)]
    [(cmp? e) (handle-cmp (dequote (cmp-op e)) (cmp-args e) d)]              
    [(if-exp? e) (handle-if (if-exp-test e) (if-exp-branch1 e)
                            (if-exp-branch2 e) d)]
    [(call? e) (handle-call (call-fn e) (call-args e) d)]))
; Test Suite
(begin-for-test
  (check-equal? (process-expr 
                 (make-call 'add5 (list 4 5)) (list MK-ADD-DEF ADD5-DEF))
                "err: undefined var"
                "true")
  (check-equal? (process-expr 
                 (make-call 'add5-or-6 (list 4)) 
                 (list MK-ADD-DEF ADD5-DEF ADD5-OR-6-DEF))
                9
                "true")
  (check-equal? (process-expr 
                 (make-call 'add5-or-6 (list 0)) 
                 (list MK-ADD-DEF ADD5-DEF ADD5-OR-6-DEF))
                6
                "true")
  (check-equal? (process-expr #t empty)
                 #t
                "true")
  (check-equal? (process-expr 
                 (make-bool 'and (list #t #f)) empty)
                #f
                "true"))

; make-lambda : Var ListOf<Def> -> Lambda or errstr?
; Creates and returns a Lambda expression from given var
; in context of given Defs else returns 
; an error string
; Examples:
(begin-for-test
  (check-equal? (make-lambda 
                 'add5 (list MK-ADD-DEF ADD5-DEF))
                (make-lam '(x) (make-call (make-call 'mk-add '(5)) '(x)))
                "true"))
; Strategy: Data Decomposition on d: Def
(define (make-lambda var defs)
  (local
    ((define d (get-fn-def var defs)))
    (if (def? d)
        (make-lam (def-params d) (def-body d))
        ERRSTR)))
; Test-suite:
(begin-for-test
  (check-equal? (make-lambda 
                 'add5 (list MK-ADD-DEF))
                ERRSTR
                "true"))

; handle-arith: Operator ListOf<Expr> ListOf<Def> -> Result
; handles an arith-expr and evaluates it. If it finds
; any errors in evaluating, returns errstr?
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-arith 
                 + '(4 5) (list MK-ADD-DEF))
                9
                "true"))
; STRATEGY: Function Composition
(define (handle-arith op args defs)
  (local
    ((define results-lst (recursive-arith args defs)))
    (if (errstr? results-lst)
        results-lst
        (calculate-result op results-lst))))
; Test-suite:
(begin-for-test
  (check-equal? (handle-arith 
                 + '(4 #t) (list MK-ADD-DEF))
                ERRSTR
                "true"))

; calculate-result: Operator ListOf<Number> -> Result
; calculates the Result for given numbers. If it finds
; any mathematical errors in evaluating, returns errstr?
; EXAMPLES:
(begin-for-test
  (check-equal? (calculate-result + '(4 5))
                9
                "true"))
; STRATEGY: Function Composition
(define (calculate-result op lst)
  (if (division-by-zero? op (rest lst))
      DIV-BY-ZERO
      (apply op lst)))
; Test-suite:
(begin-for-test
  (check-equal? (calculate-result / '(4 0))
                DIV-BY-ZERO
                "true"))

; recursive-arith: ListOf<Expr> ListOf<Def> -> ListOf<Result>
; returns the list of the Result from given Exprs.
; EXAMPLES:
(begin-for-test
  (check-equal? (recursive-arith '(4 5) empty)
                '(5 4)
                "true"))
; STRATEGY: Function Composition
(define (recursive-arith args0 defs)
  (local
    (; recursive-arith/a: ListOf<Expr> ListOf<Def> -> ListOf<Result>
     ; generates a list of given arguments in the right order that
     ; produces the right result if evaluated in that order
     ; accumulator: acc is a list that contains the results in
     ; the right order in each iteration.
     ; STRATEGY: Data Decomposition on args: ListOf<Expr>
     (define (recursive-arith/a args defs acc)
       (cond
         [(empty? args) acc]
         [else (if (number? (process-expr (first args) defs))
                   (recursive-arith/a (rest args) defs
                                      (cons (process-expr (first args) defs) 
                                            acc))
                   ERRSTR)])))
    (recursive-arith/a args0 defs empty)))
; Test-suite:
(begin-for-test
  (check-equal? (recursive-arith empty empty)
                empty
                "true"))

; division-by-zero?: Operator ListOf<Number> -> Boolean
; returns true if there is going to be a possible
; division by zero.
; EXAMPLES:
(begin-for-test
  (check-equal? (division-by-zero? / '(4 0))
                #t
                "true"))
; STRATEGY: Function Composition
(define (division-by-zero? op lst)
  (and (equal? op /)
       (member? ZERO lst)))
; Tst-suite:
(begin-for-test
  (check-equal? (division-by-zero? / '(4 5))
                #f
                "true"))



; handle-bool: Operator ListOf<Expr> ListOf<Def> ->Result
; handles make-bool exprs and evaluates them.
; returns errstr? if error ancountered, else result
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-bool and-and '(#t 5) empty)
                ERRSTR
                "true"))
; STRATEGY: Generative recursion
; Halting Measure: (length args) always decreases
(define (handle-bool op args defs)
  (local
    (; map-fn: ListOf<Expr> -> ListOf<Result>
     ; gets results from the given list of Expr
     ; STRATEGY: Function composition
     (define (map-fn arg)
       (map
        (λ(l) (process-expr l defs)) args)))
    (if (andmap boolean? (map-fn args))
        (perform-bool-op op (map-fn args))
        ERRSTR)))
; Test-suite:
(begin-for-test
  (check-equal? (handle-bool and-and '(#t #t) empty)
                #t
                "true"))

; perform-bool-op: OperatorFunction ListOf<Result>
; applies boolean function on the list given.
; Examples:
(begin-for-test
  (check-equal? (perform-bool-op and-and '(#t #t))
                #t
                "true"))
; STRATEGY: Function Composition
(define (perform-bool-op op args)
  (if (equal? and-and op)
      (foldr and-and #t args)
      (foldr or-or  #f args)))
; Test-suite:
(begin-for-test
  (check-equal? (perform-bool-op or-or '(#t #t))
                #t
                "true"))

; handle-cmp: Operator ListOf<Expr> ListOf<Def> ->Result
; handles make-cmp exprs and evaluates them.
; returns errstr? if error ancountered, else result
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-cmp < '(4 0) empty)
                #f
                "true"))
; STRATEGY: Data Decomposition on args: ListOf<Expr>
(define (handle-cmp op args defs)
  (local
    (; process-args: ListOf<Expr> -> ListOf<Result>
     ; gets results from the given list of Expr
     ; STRATEGY: Function composition
     (define (process-args arg)
       (map
        (λ(l) (process-expr l defs)) arg)))
    (if (andmap number? (process-args args))
        (apply op (process-args args))
        ERRSTR)))
; Test-suite:
(begin-for-test
  (check-equal? (handle-cmp < '(4 #t) empty)
                ERRSTR
                "true"))

    
; handle-if-test : Expr Expr ListOf<Def> -> Expr
; returns result of if-exp
; examples:
(begin-for-test
  (check-equal? (handle-if 4 4 #t empty)
                ERR-IF
                "true"))
; STRATEGY: Funciton Composition
(define (handle-if test case1 case2 defs)
  (local
    ((define (if-result tst)
       (process-expr tst defs)))
    (if (boolean? (if-result test))
        (perform-if-else-opn (if-result test) case1 case2 defs)
        ERR-IF)))
; Test-suite
(begin-for-test
  (check-equal? (handle-if #t 4 #t empty)
                4
                "true"))

; handle-if-test : Boolean Expr Expr ListOf<Def> -> Expr
; returns result of if-exp
; examples:
(begin-for-test
  (check-equal? (perform-if-else-opn #t 4 #t empty)
                4
                "true"))
; STRATEGY: Funciton Composition
(define (perform-if-else-opn test case1 case2 defs)
  (if test
      (process-expr case1 defs)
      (process-expr case2 defs)))  
; test-suite:
(begin-for-test
  (check-equal? (perform-if-else-opn #f 4 #t empty)
                #t
                "true"))

;handle-call: Expr ListOf<Expr> ListOf<Def> -> Result
; handles make-call expr and evaluates to result
; STRATEGY: Function COmposition
(define (handle-call fn args defs)
  (local
    ((define may-be-lam (process-expr fn defs)))
    (if (lambda? may-be-lam)
        (process-expr
         (recursive-subst args (lam-params may-be-lam)
                          (lam-body may-be-lam)) defs)
        ERRSTR)))
; test-suite:
(begin-for-test
  (check-equal? (handle-call "tt" '(4 5) empty)
                ERRSTR
                "true"))

; recursive-subst: ListOf<Number> ListOf<Var> Expr -> Expr or errstr?
; WHERE (length lst1 == length lst2)
; accepts 2 lists and it applies the function subst where the
; result r is chosen from lst1 and the var to be replaced x from
; lst2. Then it recursively applies the subst function with each pair
; of r and x on the given expr.
; EXAMPLES:
(begin-for-test
  (check-equal? (recursive-subst empty '(x y) (make-arith '+ '(x y)))
                (make-arith '+ '(x y))  
                "true"))  
; STRATEGY: Data Decomposition on lst1, lst2: ListOf<X>
(define (recursive-subst lst1 lst2 expr)
  (cond
    [(empty? lst1) expr]
    [else
     (if (equal? (length lst1) (length lst2))
         (subst (first lst1) (first lst2)
                (recursive-subst (rest lst1) (rest lst2) expr))
         UNDEF-VAR)]))
; Test-suite:
(begin-for-test
  (check-equal? (recursive-subst '(10 5) '(x y) (make-arith '+ '(x y)))
                (make-arith '+ '(10 5))  
                "true")
  (check-equal? (recursive-subst '(10 5) '(x) (make-arith '+ '(x y)))
                UNDEF-VAR  
                "err"))  


; get-fn-def: Var ListOf<Def> -> Maybe<Def>
; searches for the definition of the given
; var in defs and returns that def if it exists
; otherwise returns #false
; EXAMPLES:
(begin-for-test
  (check-equal? (get-fn-def 'mk-add (list MK-ADD-DEF ADD5-DEF))
                MK-ADD-DEF                 
                "found"))  
; STRATEGY: Function Composition
(define (get-fn-def fn defs)
  (local
    (; fn-def: var ListOf<Def> -> ListOf<Def>
     ; retuns a list of matching Defs that match the Var
     ; STRATEGY: Data Decomposition on d: Def
     (define (fn-def fn d)
       (filter (λ(l) (symbol=? fn (def-name l))) d)))
    (if (empty? (fn-def fn defs))
        #f
        (first (fn-def fn defs)))))
; Test-suite:
(begin-for-test
  (check-equal? (get-fn-def 'mk-add (list ADD5-DEF))
                #f                 
                "not found")) 


; dequote : ArithOp or CmpOp -> OperatorFunction
; takes a symbol and returns the respective
; logical operator.
; EXAMPLES:
(begin-for-test
  (check-equal? (dequote '+)
                +
                "true")
  (check-equal? (dequote '-)
                -
                "true")
  (check-equal? (dequote 'or)
                or-or
                "true"))
; STRATEGY: Function Composition
(define (dequote op)
  (cond
    [(symbol=? '+ op) +]
    [(symbol=? '- op) -]
    [(symbol=? '* op) *]
    [(symbol=? '/ op) /]
    [(symbol=? '= op) =]
    [(symbol=? '< op) <]
    [(symbol=? '> op) >]
    [(symbol=? 'and op) and-and]
    [(symbol=? 'or op) or-or]))
; Test-suite:
(begin-for-test
  (check-equal? (dequote '*)
                *
                "true")
  (check-equal? (dequote '/)
                /
                "true")
  (check-equal? (dequote '=)
                =
                "true")
  (check-equal? (dequote '<)
                <
                "true")
  (check-equal? (dequote '>)
                >
                "true")
  (check-equal? (dequote 'and)
                and-and
                "true"))

; returns the arithmetic identity for the
; given arithmetic operator
; EXAMPLES:
(begin-for-test
  (check-equal? (op-identity +)
                0
                "true"))
; STRATEGY: Function Composition
(define (op-identity op)
  (cond
    [(equal? op +) 0]
    [(equal? op -) 0]
    [(equal? op *) 1]
    [(equal? op /) 1]))
; Test-suite:
(begin-for-test
  (check-equal? (op-identity -)
                0
                "true")
  (check-equal? (op-identity *)
                1
                "true")
  (check-equal? (op-identity /)
                1
                "true"))

; and-and : Boolean Boolean -> Boolean
; accepts two boolean values and performs
; and operation on them
; EXAMPLES:
(begin-for-test
  (check-equal? (and-and #t #f)
                #f
                "false"))
; STRATEGY: Function Composition
(define (and-and x y)
  (and x y))
; Test-suite:
(begin-for-test
  (check-equal? (and-and #t #t)
                #t
                "true"))

; or-or : Boolean Boolean -> Boolean
; accepts two boolean values and performs
; or operation on them
; EXAMPLES:
(begin-for-test
  (check-equal? (or-or #t #f)
                #t
                "true"))
; STRATEGY: Function Composition
(define (or-or x y)
  (or x y))
; Test-suite:
(begin-for-test
  (check-equal? (or-or #f #f)
                #f
                "false"))

; subst : Result Var Expr -> Expr
; Replaces references to x in e with r.
; Does not replace x with r if x occurs in the body of a lambda
; that shadows x.
; WHERE: r has no unbound variables
; EXAMPLES:
(begin-for-test
  (check-equal? (subst 5 'x 4)
                4
                "true")
  (check-equal? (subst 5 'x #t)
                #t
                "true"))
; STRATEGY: Data Decomposition on e: Expr
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
; Test-suite:
(begin-for-test
  (check-equal? (subst 5 'x 'x)
                5
                "true")
  (check-equal? (subst 5 'x 'y)
                'y
                "true")
  (check-equal? (subst 5 'x ERRSTR)
                ERRSTR
                "true")
  (check-equal? (subst 5 'x (make-lam '(x) (make-arith '+ '(x y))))
                (make-lam '(x) (make-arith '+ '(x y)))
                "true")
  (check-equal? (subst 5 'x (make-lam '(y) (make-arith '+ '(x y))))
                (make-lam '(y) (make-arith '+ '(5 y)))
                "true")
  (check-equal? (subst 5 'x (make-bool 'and '(#t #f)))
                (make-bool 'and '(#t #f))
                "true")
  (check-equal? (subst 5 'x (make-cmp '< '(x y)))
                (make-cmp '< '(5 y))
                "true")
  (check-equal? (subst 5 'x (make-if-exp #t 'x #f))
                (make-if-exp #t '5 #f)
                "true")
  (check-equal? (subst 5 'x (make-call 'add5 (list 'x #f)))
                (make-call 'add5 (list 5 #f))
                "true"))



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


; expr->expr/no-var : Expr -> ExprNoVar
; Replaces Var in e with StaticDist.
; WHERE: there are no unbound variables in e.
; EXAMPLES:
(begin-for-test
  (check-equal? (expr->expr/no-var 10)
                10
                "true")
  (check-equal? (expr->expr/no-var #t)
                #t
                "true")
  (check-equal? (expr->expr/no-var ERRSTR)
                ERRSTR
                "true"))
; STRATEGY: Function composition
(define (expr->expr/no-var e)
  (local
    (; : -> Expr -> expr/no-var
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
                      (arith-op e) (map
                                    (λ(l) (expr->expr/no-var/a l acc))
                                    (arith-args e)))]
         [(bool? e) e]
         [(cmp? e) (make-cmp
                    (cmp-op e) (map
                                (λ(l) (expr->expr/no-var/a l acc))
                                (cmp-args e)))]  
         [(if-exp? e) (make-if-exp (expr->expr/no-var/a (if-exp-test e) acc)
                                   (expr->expr/no-var/a (if-exp-branch1 e) acc)
                                   (expr->expr/no-var/a (if-exp-branch2 e)
                                                        acc))]
         [(call? e) (make-call (expr->expr/no-var/a (call-fn e) acc)
                               (map
                                (λ(l) (expr->expr/no-var/a l acc)) 
                                (call-args e)))])))
    (expr->expr/no-var/a e empty)))
; test-suite:
(begin-for-test
  (check-equal?
   (expr->expr/no-var (make-lam '(x) 'x))
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x) (make-arith '+ '(x 5))))
   (make-lam/no-var (make-arith '+ (list '(0 0) 5)))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x) (make-bool 'and '(#t #t))))
   (make-lam/no-var (make-bool 'and (list #t #t)))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x) (make-cmp '< '(x y))))
   (make-lam/no-var (make-cmp '< (list '(0 0) empty)))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x) (make-if-exp #t #t #t)))
   (make-lam/no-var (make-if-exp #t #t #t)))
   "basic lambda")

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
;Test-suite
(begin-for-test
  (check-equal?
   (find-static-dist '(a) (list '(a b)))
   '()
   "true"))


; index-of: Any -> ListOf<Any>
; retuns the position of the given item
; in the list.
; WHERE: we assume that the item exists
; in the list.
; WHERE: index is (0-based)
; EXAMPLES:
(begin-for-test
  (check-equal?
   (index-of 1 '(1 2))
   0
   "true"))
; STRATEGY: Function Composition
(define (index-of item lst)
  (- (length lst)
     (length (memv item lst))))
; Test-suite:
(begin-for-test
  (check-equal?
   (index-of 2 '(1 2))
   1
   "true"))

; expr=? : Expr Expr -> Boolean
; Returns true if e1 and e2 are structurally equivalent, up to some
; renaming of variable names.
; EXAMPLES:
(begin-for-test
  (check
   expr=?
   (make-lam '(x) 'x)
   (make-lam '(y) 'y)
   "equivalent basic lambdas"))
; STRATGY: Function Composition
(define (expr=? e1 e2)
  (equal? (expr->expr/no-var e1)
          (expr->expr/no-var e2)))
; Test-suite
(begin-for-test
  (check-false
   (expr=?
    (make-lam '(x y) (make-call 'x '(y)))
    (make-lam '(y x) (make-call 'x '(y))))
   "not equivalent")
  (check
   expr=?
   (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))
   (make-lam '(x) (make-lam '(y) (make-call 'x '(y))))
   "equivalent nested-lambdas"))


; add-at-last : Any ListOf<Any> -> ListOf<Any>
; Adds the item at the last of the given list
; EXAMPLES: 
(begin-for-test
  (check-equal? (add-at-last 3 (list 0 1 2))
                (list 0 1 2 3)
                "true"))
; STRATEGY: Function Composition
(define (add-at-last item lst)
  (append
   lst
   (list item)))