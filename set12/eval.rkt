#lang racket
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 15)

(provide mk-Program%)
(provide mk-Expr%)
(provide Program<%>)
(provide Expr<%>)
(provide Result<%>)

;;=============================================================================
; A Var is a Symbol, representing PDPLang variable.

;Data example
(define var_ex 'f)

;;=============================================================================
; An ErrString is a String, representing a PDPLang error message.

;Data example
(define INVALID_ARITH_ARGUMENTS "Invalid arguments for Arithmetic")
(define INVALID_BOOL_ARGUMENTS "Invalid arguments for Boolean")
(define INVALID_CMP_ARGUMENTS "Invalid arguments for Comparator")
(define INVALID_IF_EXP_ARGUMENTS "Invalid arguments for If-expression")
(define UNDEFINED_FUNCTION "Undefined function")
(define INVALID_CALL_ARGUMENTS "Function doesnt evaluate to lambda")
(define INVALID_NUMBER_OF_ARGUMENTS "Invalid number of arguments")
(define APP_ERR "Apply error - not lambda")

;;=============================================================================
; A UniqueListOf<X> is a ListOf<X>
; WHERE: none of the elements of the list are equal? to each other

;Data example
(define uloParam (list 'f 'g))

;;=============================================================================
; A 2ListOf<X> is a (cons X (cons X ListOf<X>))
; Represents a list of 2 or more elements.

; A 2ListOfExpr (2LoX) is a 2ListOf<Expr>
; TEMPLATE
; 2lox-fn : 2ListOfExpr -> ???
;(define (2lox-fn 2lox)
;  (... (expr-fn (first 2lox)) ... (nelst-fn (rest 2lox)) ...))

;Data example
(define 2loexpr (list 3 'f))

;;=============================================================================
; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define-struct lam (params body) #:transparent)

;Template
;(define (lam-fn l)
;  (.. (ulox-fn (lam-params l)) ... (expr-fn (lam-body l)) ...))

;Data example
(define lambda-expr (make-lam '(x y) 5))

;;=============================================================================
; A Param is a Var, representing a function parameter.

;Data example
(define param 'f)

;;=============================================================================
; An Expr is one of:
; - Number
; - Boolean
; - Var
; - ErrString
; - Lambda
; - (make-arith ArithOp 2ListOf<Expr>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<Expr>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<Expr>)     ; a comparison expression
; - (make-if-exp BoolExpr Expr Expr) ; an if conditional
; - (make-call Expr ListOf<Expr>) ; a function call
; Represents a PDPLang expression.

(define-struct arith (op args) #:transparent)
(define-struct bool (op args) #:transparent)
(define-struct cmp (op args) #:transparent)
(define-struct if-exp (test branch1 branch2) #:transparent)
(define-struct call (fn args) #:transparent)

;Template
;(define (expr-fn e)
;  (cond
;    [(number? e) ...]
;    [(boolean? e) ...]
;    [(symbol? e) ...]
;    [(string? e) ...]
;    [(lam? e) (... (ulox-fn (lam-params e)) ... (expr-fn (lam-body e)) ...)]
;    [(arith? e) (... (arithop-fn (arith-op e))...(2lox-fn (arith-args e)) ...)]
;    [(bool? e) (...(boolop-fn (bool-op e)) ... (2lox-fn (bool-args e)) ...)]
;    [(cmp? e) (...(cmpop-fn (cmp-op e)) ... (2lox-fn (cmp-args e)) ...)]
;    [(if-exp? e) (...  (expr-fn (if-exp-test e)) ... 
;                       (expr-fn (if-exp-branch1 e)) ... 
;                       (expr-fn (if-exp-branch2 e)) ...)]
;    [(call? e) (... (expr-fn (call-fn e)) ... (lox-fn (call-args e)) ...)]))


; A ListOf<X> is a ListOf<X>
; A ListOfExpr (LoX) is a ListOf<Expr>
; Template
; lox-fn : ListOfExpr -> ???
;(define (lox-fn lox)
;  (... (expr-fn (first lox)) ... (lox-fn (rest lox)) ...))

;Data examples
(define num-expr 4)
(define boolean-expr #true)
(define var-expr 'f)                  
(define errs-expr "Invalid Args")
(define lam-expr (make-lam '(x y) (make-arith '- '(x y))))
(define arith-expr (make-arith '+ '(2 3 4)))
(define bool-expr (make-bool 'and (list #true #false)))
(define cmp-expr (make-cmp '< '(4 5)))
(define if-expr (make-if-exp cmp-expr #true #false))
(define call-expr (make-call lam-expr '(3 4)))

;;=============================================================================
; An ArithOp is one of:
; - '+
; - '-
; - '*
; - '/
; Represents an arithmetic operation in PDPLang

(define ADD '+)
(define SUB '-)
(define MUL '*)
(define DIV '/)

; <arithop-predicates> : ArithOp -> Boolean
; Returns true if op is the ArithOp indicated by function name
; STRATEGY: function composition
(define (add? op) (symbol=? op ADD))
(define (sub? op) (symbol=? op SUB))
(define (mul? op) (symbol=? op MUL))
(define (div? op) (symbol=? op DIV))

; TEMPLATE
; arithop-fn: ArithOp -> ???
;(define (arithop-fn op)
;  (cond
;    [(add? op) ...]
;    [(sub? op) ...]
;    [(mul? op) ...]
;    [(div? op) ...]))

;;=============================================================================
; A BoolOp is one of:
; - 'and
; - 'or
; Represents a boolean operation in PDPLang
(define AND 'and)
(define OR 'or)

; <boolop-predicates> : BoolOp -> Boolean
; Returns true if op is the BoolOp indicated by function name
; STRATEGY: function composition
(define (and? op) (symbol=? op AND))
(define (or? op) (symbol=? op OR))

; TEMPLATE
; boolop-fn: BoolOp -> ???
;(define (boolop-fn op)
;  (cond
;    [(and? op) ...]
;    [(or? op) ...]))

;;=============================================================================
; A CmpOp is one of:
; - '=
; - '<
; - '>
; Represents a comparison operation in PDPLang
(define EQUALS '=)
(define LESSER '<)
(define GREATER '>)

; <cmpop-predicates> : CmpOp -> Boolean
; Returns true if op is the CmpOp indicated by function name
; STRATEGY: function composition
(define (equals? op) (symbol=? op EQUALS))
(define (lesser? op) (symbol=? op LESSER))
(define (greater? op) (symbol=? op GREATER))

; TEMPLATE
; cmpop-fn: CmpOp -> ???
;(define (cmpop-fn op)
;  (cond
;    [(equals? op) ...]
;    [(lesser? op) ...]
;    [(greater? op) ...]))

;;=============================================================================
; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define-struct def (name params body) #:transparent)

; A FnName is a Var, representing a function name.

;Template
;(define (def-fn d)
;  (... (def-name d) ...(ulox-fn (def-params d)) ...(expr-fn (def-body d)) ...))

;Data exmaples
(define def1 (make-def 'f '(x) (make-arith '* '(x 2))))

;;=============================================================================
; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name

; TEMPLATE
;(define (program-fn p)
;  (cond
;    [(empty? p) '()]
;    [(not (def? (first p))) (... (expr-fn (first p)) ... 
;                                 (program-fn (rest p)) ...)]
;    [(def? (first p)) (... (def-name (first p)) 
;                           (ulox-fn (def-params (first p)))
;                           (expr-fn (def-body (first p))) ...
;                           (program-fn (rest p)) ...)]))

;Data examples
(define program1 '())
(define program2 (list (make-arith '+ '(2 3)) (make-cmp '< '(4 5))))
(define program3 (list (make-def 'f '(x) (make-arith '* '(x 2))) 'f "error")) 

;;=============================================================================
; A LamNoVar is a (make-lam/no-var ExprNoVar)
; INTERP: represents a lambda without a Var in it.
(define-struct lam/no-var (body) #:transparent)

;Template
;(define (lam/no-var-fn l)
;  (... (exprnovar-fn (lam/no-var-body l)) ...))

;Data examples
(define lam/no-var-ex (make-lam/no-var '(0 0)))

;;=============================================================================
; A StaticDist is a (list Depth Index)
; Represents a variable reference
; where depth is number of additional lambdas between this var ref and the
; lambda for which this variable is a parameter,
; and index is the (0-based) position of this variable in that lambda's
; parameter list.

; A Depth is a Natural
; An Index is a Natural

;Data examples
(define staticDist (list 1 0))

;;=============================================================================
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

;Template
;(define (exprnovar-fn e)
;  (cond
;    [(number? e) ...]
;    [(boolean? e) ...]
;    [(list? e) ...]
;    [(string? e) ...]
;    [(lam/no-var? e) (... (ulox-fn (lam-params e)) ... 
;                          (exprnovar-fn (lam-body e)) ...)]
;    [(arith? e) (...(arithop-fn (arith-op e))... (2lox-fn (arith-args e)) ...)]
;    [(bool? e) (...(boolop-fn (bool-op e)) ... (2lox-fn (bool-args e)) ...)]
;    [(cmp? e) (...(cmpop-fn (cmp-op e)) ... (2lox-fn (cmp-args e)) ...)]
;    [(if-exp? e) (...  (exprnovar-fn (if-exp-test e)) ... 
;                       (exprnovar-fn (if-exp-branch1 e)) ... 
;                       (exprnovar-fn (if-exp-branch2 e)) ...)]
;    [(call? e) (... (exprnovar-fn (call-fn e))...(lox-fn (call-args e)) ...)]))

;Data examples
(define staticDist-expr staticDist)
(define lambda/no-var-expr (make-lam/no-var '(0 0)))
(define call/no-var-expr
  (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0))))))

;;=============================================================================
; A StaticInfoInterface<%> represents the lambda variables with index and depth
; representing values with which name is replaced.

(define StaticInfoInterface<%>
  (interface ()
    ; get-name : -> Symbol
    ; Returns the name of the StaticInfo object
    get-name
    ; get-depth : -> Natural
    ; Returns the depth of the StaticInfo object
    get-depth
    ; get-index : -> Natural
    ; Returns the index of the StaticInfo object
    get-index))

(define StaticInfo%
  (class* object% (StaticInfoInterface<%>)
    (init-field name ; Symbol, represents the name of the definition
                depth ; Natural, represents number of additional lambdas between
                ;this var ref and the lambda for which name is a parameter
                index) ;Natural, position of name in that lambda's
                ; parameter list.
    
    ; get-name : -> Symbol
    (define/public (get-name)
      name)
    
    ; get-depth : -> Natural
    (define/public (get-depth)
      depth)
    
    ; get-index : -> Natural
    (define/public (get-index)
      index)
    
    (super-new)))

;;=============================================================================
(define Result<%>
  (interface ()
    ; app : ListOf<Result<%>> ListOf<Def<%>> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    app))

;;=============================================================================
(define Expr<%>
  (interface ()
    ; eval : ListOf<Def<%>> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    eval
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    subst
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    to-expr
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    to-expr/no-var
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the 
    ; given Expr<%>
    expr=?
    ; set-loSI : ListOf<StaticInfo%> -> Expr<%>
    ; Returns new Expr<%> by setting the loSI parameter 
    set-loSI))

;;=============================================================================

(define Number%
  (class* object% (Expr<%> Result<%>)
    (init-field val) ; Number, Represents a PDPLang expression.
    
    ; eval : ListOf<Def<%>> -> Number<%>
    (define/public (eval loDef)
      this)
    
    ; subst : Result<%> Var -> Number<%>
    (define/public (subst resObj var)
      this)
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      val)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      val)
    
    ; set-loSI : ListOf<StaticInfo%> -> Number<%>
    (define/public (set-loSI params)
      this)
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send this to-expr/no-var)
            (send expObj to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def<%>> -> Result<%>
    (define/public (app lor lod)
      (new ErrString% [val APP_ERR]))
    
    (super-new)))

;;=============================================================================

(define Boolean%
  (class* object% (Expr<%> Result<%>)
    (init-field val) ; Boolean, Represents a PDPLang expression.
    
    ; eval : ListOf<Def<%>> -> Boolean<%>
    (define/public (eval loDef)
      this)
    
    ; subst : Result<%> Var -> Boolean<%>
    (define/public (subst resObj var)
      this)
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      val)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      val)
    
    ; set-loSI : ListOf<StaticInfo%> -> Boolean<%>
    (define/public (set-loSI params)
      this)
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send this to-expr/no-var)
            (send expObj to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def<%>> -> Result<%>
    (define/public (app lor lod)
      (new ErrString% [val APP_ERR]))
    
    (super-new))) 

;;=============================================================================

(define Var%
  (class* object% (Expr<%>)
    (init-field val  ; Var, Represents a PDPLang expression.
                loSI); ListOf<StaticInfo%>,Holds info required to convert to 
                      ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (local
        ((define matched-def (filter (λ (x) (symbol=? val (send x get-name))) 
                                     loDef)))
        
        (if (empty? matched-def) 
            (new ErrString% [val UNDEFINED_FUNCTION])
            (def-to-lambda (first matched-def)))))
    
    ;def-to-lambda : Def<%> -> Lambda<%>
    ;Returns Lambda for given Def.
    (define (def-to-lambda def)
      (new Lambda% [params (send def get-params)] [body (send def get-body)]
           [loSI '()]))
    
    ; subst : Result<%> Var -> Expr<%>
    (define/public (subst resObj var)
      (if (symbol=? var val) resObj this))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      val)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (local
        ((define match (first (filter 
                               (λ (x) (symbol=? (send x get-name) val)) 
                               loSI))))
        
        (list (send match get-depth) (send match get-index))))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    ; set-loSI : ListOf<StaticInfo%> -> Var<%>
    (define/public (set-loSI params)
      (new Var% [val val] [loSI params]))
    
    (super-new)))

;;=============================================================================

(define ErrString%
  (class* object% (Expr<%> Result<%>)
    (init-field val) ; String, Represents a PDPLang expression.
    
    ; eval : ListOf<Def<%>> -> ErrString<%>
    (define/public (eval loDef)
      this)
    
    ; subst : Result<%> Var -> ErrString<%>
    (define/public (subst resObj var)
      this)
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      val)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      val)
    
    ; set-loSI : ListOf<StaticInfo%> -> ErrString<%>
    (define/public (set-loSI params)
      this)
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send this to-expr/no-var)
            (send expObj to-expr/no-var)))
     
    ; app : ListOf<Result<%>> ListOf<Def<%>> -> Result<%>
    (define/public (app lor lod)
      (new ErrString% [val APP_ERR]))
      
    (super-new)))

;;=============================================================================

(define Arith%
  (class* object% (Expr<%>)
    (init-field op   ; ArithOp, represents arithmetic operation
                args ; ListOf<Expr<%>>, Represents arguments to be processed
                loSI); ListOf<StaticInfo%>,Holds info required to convert to 
                      ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (local
        ((define evaluated-args (map (λ (x) (send x eval loDef)) args))
         (define err-msg (filter (λ (x) (string? (send x to-expr))) 
                                 evaluated-args)))
        
        (if (not (empty? err-msg)) (first err-msg)
            (get-arith-val evaluated-args))))
    
    ; get-arith-val : ListOf<Result<%>> -> Result<%>
    ; Peforms op Operation on ListOf<Result<%>>
    ; Returns ErrString in case of an invalid argument.
    (define (get-arith-val loArgs)
      (if (valid-arith-args? loArgs) 
          (new Number% [val (calculate-arith op (map (λ(x) (send x to-expr)) 
                                                     loArgs))])
          (new ErrString% [val INVALID_ARITH_ARGUMENTS])))
    
    
    ; valid-arith-args? : ListOf<Result<%>> -> Boolean
    ; Returns true if given list of arguments contains only numbers
    ; STRATEGY: data decomposition on op : ArithOp
    (define (valid-arith-args? loArgs)
      (cond
        [(or (add? op) (sub? op) (mul? op))
         (empty? (filter (λ (x) (not (number? (send x to-expr)))) loArgs))]
        [(div? op) 
         (empty? (filter (λ (x) (or (not (number? (send x to-expr))) 
                                    (zero? (send x to-expr)))) loArgs))]))
    
    
    ; calculate-arith : ArithOp ListOf<Number> -> Number
    ; Returns result of performing given Arithmetic Operation op on loa
    ; STRATEGY: data decomposition on op : ArithOp
    (define (calculate-arith op loa)
      (cond
        [(add? op) (foldl + 0 loa)] 
        [(sub? op) (foldl (λ(x y) (- y x)) (get-base-for-sub loa) loa)]
        [(mul? op) (foldl * 1 loa)]
        [(div? op) (foldl (λ(x y) (/ y x)) (get-base-for-div loa) loa)]))
    
    
    ; get-base-for-sub: ListOf<Number> -> Number
    ; Returns a base value for foldl operation for subraction operation
    ; STRATGEY: data decomposition on loa: ListOf<Number>
    ; Ex. for input '(3 5 7) output is 6
    (define (get-base-for-sub loa)
      (* (first loa) 2))
    
    ; get-base-for-div: ListOf<Number> -> Number
    ; Returns a base value for foldl operation for Division operation
    ; STRATGEY: data decomposition on loa: ListOf<Number>
    ; Ex. for input '(3 5 7) output is 9
    (define (get-base-for-div loa)
      (* (first loa) (first loa)))    
    
    ; subst : Result<%> Var -> Arith<%>
    (define/public (subst resObj var)
      (new Arith% [op op] [args (map (λ (x) (send x subst resObj var)) args)]
           [loSI loSI]))   
    
    ; set-loSI : ListOf<StaticInfo%> -> Arith<%>
    (define/public (set-loSI params)
      (new Arith% [op op] [args args] [loSI params]))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-arith op (map (λ (x) (send x to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-arith op (map (λ (x) (send (send x set-loSI loSI)
                                       to-expr/no-var)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    (super-new)))

;;=============================================================================

(define Lambda%
  (class* object% (Expr<%> Result<%>)
    (init-field params ;UniqueListOf<Param>, represents list of parameters
                body ; Expr<%>, represents body of PDP lambda expression
                loSI) ; ListOf<StaticInfo%>,Holds info required to convert to 
                       ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      this)
    
    ; subst : Result<%> Var -> Lambda<%>
    (define/public (subst resObj var)
      (if (member? var params) this
          (new Lambda% [params params] [body (send body subst resObj var)]
               [loSI loSI])))
    
    ; member? : Var ListOf<Params> -> Boolean
    ; Returns true if var is a member of given list
    (define (member? var lop)
      (not (zero? (length (filter (λ (x) (symbol=? var x)) lop)))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-lam params (send body to-expr)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-lam/no-var (send (send body set-loSI 
                                   (add-to-loSI (increment-depth loSI) 
                                                params))
                             to-expr/no-var)))
    
    ; set-loSI : ListOf<StaticInfo%> -> Lambda<%>
    (define/public (set-loSI siParams)
      (new Lambda% [params params] [body body] [loSI siParams]))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    ; add-to-loSI : ListOf<StaticInfo%> ListOf<Params> -> ListOf<StaticInfo%>
    ; Adds list of params as StaticInfo to the list loSI 
    (define (add-to-loSI loSI loParams)
      (local
        (; add-to-loSI:ListOf<StaticInfo%> ListOf<Params> -> ListOf<StaticInfo%>
         ; Adds list of params as StaticInfo% to the list loSI 
         ; WHERE : position is the number of params 
         ; STRATEGY: data decomposition loParams/0 : ListOf<Params>
         (define (add-to-loSI/0 loSI/0 loParams/0 position)
           (cond
             [(empty? loParams/0) loSI/0]
             [else (add-to-loSI/0
                    (cons (new StaticInfo% [name (first loParams/0)][depth 0]
                               [index position]) 
                          loSI/0)
                    (rest loParams/0) (add1 position))])))
        
        (add-to-loSI/0 loSI loParams 0)))
    
    
    ; increment-depth : ListOf<StaticInfo%> -> ListOf<StaticInfo%>
    ; Increments depth of all the elements in given loSI
    (define (increment-depth loSI)
      (map (λ (x) (new StaticInfo% [name (send x get-name)]
                       [depth (add1 (send x get-depth))]
                       [index (send x get-index)])) loSI))
     
    ; app : ListOf<Result<%>> ListOf<Def<%>> -> Result<%>
    (define/public (app loArgs loDef)
      (if (not (equal? (length loArgs) (length params)))
          (new ErrString% [val INVALID_NUMBER_OF_ARGUMENTS])
          (send (subst-params loArgs params body) eval loDef)))
      
    ; subst-params : ListOf<Result<%>> UniqueListOf<Param> Expr<%> -> Expr<%>
    ; Substitutes ListOf<Result> for UniqueListOf<Param> in Expr.
    ; STRATEGY: data decomposition on loArgs:ListOf<Result<%>>
    ; and loParams:UniqueListOf<Param>
    (define (subst-params loArgs loParams body)
      (cond
        [(empty? loArgs) body]
        [else (subst-params 
               (rest loArgs) (rest loParams)
               (send body subst (first loArgs) (first loParams)))]))   

    (super-new)))

;;=============================================================================

(define Bool%
  (class* object% (Expr<%>)
    (init-field op   ; BoolOp, represents boolean operation
                args ; ListOf<Expr<%>>, Represents arguments to be processed
                loSI); ListOf<StaticInfo%>,Holds info required to convert to 
                       ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (local
        ((define evaluated-args (map (λ (x) (send x eval loDef)) args))
         (define err-msg (filter (λ (x) (string? (send x to-expr))) 
                                 evaluated-args)))
        
        (if (not (empty? err-msg)) (first err-msg)
            (get-bool-val evaluated-args))))
    
    ; get-bool-val : BoolOp ListOf<Result<%>> -> Result<%>
    ; Peforms op Bool Operation on ListOf<Result<%>>
    (define (get-bool-val loArgs)
      (if (empty? (filter (λ (x) (not (boolean? (send x to-expr)))) loArgs)) 
          (new Boolean% [val (calculate-bool (map (λ(x) (send x to-expr))
                                                  loArgs))])
          (new ErrString% [val INVALID_BOOL_ARGUMENTS])))   
    
    ; calculate-bool : BoolOp ListOf<Boolean> -> Boolean
    ; Returns result of performing given Boolean Operation op on loa
    ; STRATEGY: data decomposition on op : BoolOp
    (define (calculate-bool loa)
      (cond
        [(and? op) (andmap identity loa)]
        [(or? op) (ormap identity loa)]))
    
    ; subst : Result<%> Var -> Bool<%>
    (define/public (subst resObj var)
      (new Bool% [op op] [args (map (λ (x) (send x subst resObj var)) args)]
           [loSI loSI]))   
    
    ; set-loSI : ListOf<StaticInfo%> -> Bool<%>
    (define/public (set-loSI params)
      (new Bool% [op op] [args args] [loSI params]))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-bool op (map (λ (x) (send x to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-bool op (map (λ (x) (send (send x set-loSI loSI)
                                      to-expr/no-var)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    (super-new)))

;;=============================================================================

(define Cmp%
  (class* object% (Expr<%>)
    (init-field op   ; CmpOp, represents comparator operation
                args ; ListOf<Expr<%>>, Represents arguments to be processed
                loSI); ListOf<StaticInfo%>,Holds info required to convert to 
                       ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (local
        ((define evaluated-args (map (λ (x) (send x eval loDef)) args))
         (define err-msg (filter (λ (x) (string? (send x to-expr))) 
                                 evaluated-args)))
        
        (if (not (empty? err-msg)) (first err-msg)
            (get-cmp-val evaluated-args))))
    
    ; get-cmp-val : ListOf<Result> ->  MaybeOuput<Boolean>
    ; Peforms given op Compare Operation on ListOf<Result>
    ; Returns ErrString in case of an invalid argument.
    (define (get-cmp-val loArgs)
      (if (empty? (filter (λ (x) (not (number? (send x to-expr)))) loArgs)) 
          (new Boolean% [val (calculate-cmp (map (λ(x) (send x to-expr)) 
                                                 loArgs))])
          (new ErrString% [val INVALID_CMP_ARGUMENTS])))
    
    ; calculate-cmp :  ListOf<Result> -> Boolean
    ; Returns result of performing given Compare Operation op on loa
    (define (calculate-cmp loa)
      (local
        (; calculate-cmp/0: ListOf<Result> Boolean -> Boolean
         ; Returns result of performing given Compare Operation op on loa
         ; WHERE res holds the output of op operation performed on all
         ; values between loa and loa/0
         ; STRATEGY: data decomposition on loa : ListOf<Result>
         (define (calculate-cmp/0 loa/0 res)
           (cond
             [(empty? (rest loa/0)) res]
             [else (calculate-cmp/0 
                    (rest loa/0) (performCmpOperation (first loa/0) 
                                                      (second loa/0) res))])))
        
        (calculate-cmp/0 loa #true)))
    
    ; performCmpOperation : Number Number Boolean -> Boolean
    ; Returns result of performing given Compare Operation op on val1 and val2
    ; STRATEGY: data decomposition on op:CmpOp
    (define (performCmpOperation val1 val2 res)
      (cond
        [(equals? op) (and res (= val1 val2))]
        [(lesser? op) (and res (< val1 val2))]
        [(greater? op) (and res (> val1 val2))]))
    
    
    ; subst : Result<%> Var -> Cmp<%>
    (define/public (subst resObj var)
      (new Cmp% [op op] [args (map (λ (x) (send x subst resObj var)) args)]
           [loSI loSI]))    
    
    ; set-loSI : ListOf<StaticInfo%> -> Cmp<%>
    (define/public (set-loSI params)
      (new Cmp% [op op] [args args] [loSI params]))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-cmp op (map (λ (x) (send x to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-cmp op (map (λ (x) (send (send x set-loSI loSI)
                                     to-expr/no-var)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    (super-new)))

;;=============================================================================

(define IfExp%
  (class* object% (Expr<%>)
    (init-field test ;Expr<%>,deciding factor based on which result is evaluated
                branch1 ;Expr<%>, evaluates to result if test is true
                branch2 ;Expr<%>, evaluates to result if test is false
                loSI); ListOf<StaticInfo%>,Holds info required to convert to 
                       ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (local
        ((define eval-test (send (send test eval loDef) to-expr)))
        
        (if (boolean? eval-test) 
            (get-if-exp-val eval-test loDef)
            (new ErrString% [val INVALID_IF_EXP_ARGUMENTS]))))
    
    ; get-if-exp-val : Result ListOf<Def> -> Result
    ; Evaluate branch1 or branch2 based on test.
    (define (get-if-exp-val test loDef)
      (if test (send branch1 eval loDef) (send branch2 eval loDef)))    
    
    ; subst : Result<%> Var -> IfExp<%>
    (define/public (subst resObj var)
      (new IfExp% [test (send test subst resObj var)] 
           [branch1 (send branch1 subst resObj var)] 
           [branch2 (send branch2 subst resObj var)]
           [loSI loSI]))
    
    ; set-loSI : ListOf<StaticInfo%> -> IfExp<%>
    (define/public (set-loSI params)
      (new IfExp% [test test] [branch1 branch1] 
           [branch2 branch2] [loSI params]))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-if-exp (send test to-expr) (send branch1 to-expr) 
                   (send branch2 to-expr)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-if-exp (send (send test set-loSI loSI) to-expr/no-var)
                   (send (send branch1 set-loSI loSI) to-expr/no-var)
                   (send (send branch2 set-loSI loSI) to-expr/no-var)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    (super-new)))

;;=============================================================================

(define Call%
  (class* object% (Expr<%>)
    (init-field fn ; Expr<%>, represents the name
                args ; ListOf<Expr<%>>,list of arguments to be processed
                loSI) ; ListOf<StaticInfo%>,Holds info required to convert to 
                      ;ExprNoVar
    
    
    ; eval : ListOf<Def<%>> -> Result<%>
    (define/public (eval loDef)
      (send (send fn eval loDef) app 
            (map (λ (x) (send x eval loDef)) args) loDef))
    
    ; subst : Result<%> Var -> Call<%>
    (define/public (subst resObj var)
      (new Call% [fn (send fn subst resObj var)] 
           [args (map (λ (x) (send x subst resObj var)) args)] 
           [loSI loSI]))
    
    ; set-loSI : ListOf<StaticInfo%> -> Call<%>
    (define/public (set-loSI params)
      (new Call% [fn fn] [args args] [loSI params]))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-call (send fn to-expr) (map (λ (x) (send x to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (make-call (send (send fn set-loSI loSI) to-expr/no-var)
                 (map (λ (x) (send (send x set-loSI loSI)
                                   to-expr/no-var)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expObj)
      (send (new CmpExpNoVar%) compare 
            (send (send this set-loSI loSI) to-expr/no-var)
            (send (send expObj set-loSI loSI) to-expr/no-var)))
    
    (super-new)))

;;=============================================================================
(define CmpExpNoVarInt<%>
  (interface ()
    ; compare : ExprNoVar ExprNoVar -> Boolean
    ; Returns true if given ExprNoVar are structurally equivalent
    compare))

(define CmpExpNoVar%
  (class* object% (CmpExpNoVarInt<%>)
    
    ; compare : ExprNoVar ExprNoVar -> Boolean
    ; Returns true if val1 and val2 are structurally equivalent
    ; STRATEGY: data decomposition on val1, val2 : ExprNoVar
    (define/public (compare val1 val2)
      (cond
        [(number? val1) (and (number? val2) (equal? val1 val2))]
        [(boolean? val1) (and (boolean? val2) (equal? val1 val2))]
        [(list? val1) (if (list? val2) (compare-list val1 val2) #false)] 
        [(string? val1) (and (string? val2) (equal? val1 val2))]
        
        [(lam/no-var? val1) (if (lam/no-var? val2)
                                (compare (lam/no-var-body val1) 
                                         (lam/no-var-body val2))
                                #false)]
        
        [(arith? val1) (if (arith? val2) (compare-expr (arith-op val1) 
                                                       (arith-op val2)
                                                       (arith-args val1)
                                                       (arith-args val2)) 
                           #false)]
        
        [(bool? val1) (if (bool? val2) (compare-expr (bool-op val1) 
                                                     (bool-op val2)
                                                     (bool-args val1)
                                                     (bool-args val2)) #false)]
        
        [(cmp? val1) (if (cmp? val2) (compare-expr (cmp-op val1) 
                                                   (cmp-op val2)
                                                   (cmp-args val1)
                                                   (cmp-args val2)) #false)]
        
        [(if-exp? val1) 
         (if (if-exp? val2) (and 
                             (compare (if-exp-test val1) (if-exp-test val2))
                             (compare (if-exp-branch1 val1) 
                                      (if-exp-branch1 val2))
                             (compare (if-exp-branch2 val1) 
                                      (if-exp-branch2 val2)))
             #false)]
        
        [(call? val1) (if (call? val2) (and 
                                        (compare (call-fn val1) (call-fn val2))
                                        (compare-list (call-args val1) 
                                                      (call-args val2))) 
                          #false)]))
    
    
    ; compare-expr : Operator Operator ListOf<Expr> ListOf<Expr> -> Boolean
    ; Returns true if val1 and val2 are structurally equivalent
    (define (compare-expr op1 op2 arg1 arg2)
      (if (symbol=? op1 op2) (compare-list arg1 arg2) #false))
    
    
    ; compare-list : ListOf<Expr> ListOf<Expr> -> Boolean
    ; Returns true if all elements of loe1 and loe2 are equivalent
    (define (compare-list loe1 loe2)
      (if (not (equal? (length loe1) (length loe2))) #false
          (compare-list-values loe1 loe2)))
    
    
    ; compare-list-values : ListOf<Expr> ListOf<Expr> -> Boolean
    ; Returns true if all elements of loe1 and loe2 are equivalent
    ; STRATEGY: data decomposition on loe1, loe2 : ListOf<Expr> 
    (define (compare-list-values loe1 loe2)
      (cond
        [(empty? loe1) #true]
        [else (and (compare (first loe1) (first loe2)) 
                   (compare-list-values (rest loe1) (rest loe2)))]))
    
    (super-new)))

;;=============================================================================
(define Program<%>
  (interface ()
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions
    ; in the program.
    eval))

(define Prog%
  (class* object% (Program<%>)
    (init-field loP); ListOfProgram<%>, represents the input to the interpretor
        
    ; eval : -> ListOf<Result<%>>
    ; STRATEGY: generative recurrsion
    ; TERMINATION ARGUMENT: eval does not terminate for an expr of type Call<%>
    ; whose body invokes another call object with same body, without a 
    ; terminating condition
    
    ; Example for non-terminating case
    ;(define self-rec (new Def% [name 'rec] [params '(x)] 
    ;                           [body (new Call% [fn 'rec] [args '(5)])]))
    ;(eval (list self-rec (new Call% [fn 'rec] [args '(5)])))
    (define/public (eval)
      (local
        ((define loDef (filter (λ (x) (is-a? x Def%)) loP))
         (define loExpr (filter (λ (x) (is-a? x Expr<%>)) loP)))
        
        (if (empty? loExpr)
            (list '())
            (map (λ (x) (send x eval loDef)) loExpr))))
    
    (super-new)))

;;=============================================================================
(define Definition<%>
  (interface ()
    ; get-name : -> Symbol
    ; Returns the name of the Definition object
    get-name
    ; get-params : -> UniqueListOf<Param>
    ; Returns the parameters of the Definition object
    get-params
    ; get-body : -> Expr<%>
    ; Returns the body of the Definition object
    get-body))

(define Def%
  (class* object% (Definition<%>)
    (init-field name ; Symbol, represents the name of the definition
                params ; UniqueListOf<Param>, represents the paramters
                body) ;Expr<%>, represents the body
    
    ; get-name : -> Symbol
    (define/public (get-name)
      name)
    
    ; get-params : -> UniqueListOf<Param>
    (define/public (get-params)
      params)
    
    ; get-body : -> Expr<%>
    (define/public (get-body)
      body)
    
    (super-new)))

;;=============================================================================
; mk-Expr% : Expr -> Expr<%>
; Converts given Expr to corresponding Expr<%> object
; Strategy : data decomposition on e : Expr
(begin-for-test
  (check-equal? (is-a? (mk-Expr% (make-arith '/ '(10 2))) Arith%) 
                #t
                "Returns an arith object"))

(define (mk-Expr% e) 
  (cond
    [(number? e) (new Number% [val e])]
    [(boolean? e) (new Boolean% [val e])]
    [(symbol? e) (new Var% [val e] [loSI '()])] 
    [(string? e) (new ErrString% [val e])]
    [(lam? e) (new Lambda% [params (lam-params e)] 
                   [body (mk-Expr% (lam-body e))] [loSI '()])]
    [(arith? e) (new Arith% [op (arith-op e)] 
                     [args (map (λ (x) (mk-Expr% x)) (arith-args e))] 
                     [loSI '()])]
    [(bool? e) (new Bool% [op (bool-op e)] 
                    [args (map (λ (x) (mk-Expr% x)) (bool-args e))] 
                    [loSI '()])]
    [(cmp? e) (new Cmp% [op (cmp-op e)] 
                   [args (map (λ (x) (mk-Expr% x)) (cmp-args e))] 
                   [loSI '()])]
    [(if-exp? e) (new IfExp% [test (mk-Expr% (if-exp-test e))] 
                      [branch1 (mk-Expr% (if-exp-branch1 e))] 
                      [branch2 (mk-Expr% (if-exp-branch2 e))] 
                      [loSI '()])]
    [(call? e) (new Call% [fn (mk-Expr% (call-fn e))] 
                    [args (map (λ (x) (mk-Expr% x)) (call-args e))]  
                    [loSI '()])]))

;;=============================================================================
; mk-Program% : Program -> Program<%>
; Converts given Program to corresponding Program<%> object
; Strategy : function composition
(begin-for-test
  (check-equal? (is-a? (mk-Program% (list (make-arith '/ '(10 2)))) Prog%) 
                #t
                "Returns a prog object"))

(define (mk-Program% p)
  (new Prog% [loP (get-loP p)]))

;;=============================================================================

; get-loP : Program -> ListOf<Program<%>>
; Converts all programs to their corresponding object
; Strategy: data decmposition on p : Program
(begin-for-test
  (check-equal? (is-a? (first (get-loP (list (make-arith '/ '(10 2))))) Arith%) 
                #t
                "Returns a Arith object"))

(define (get-loP p)
  (cond
    [(empty? p) '()]
    [(def? (first p)) (cons (new Def% [name (def-name (first p))] 
                                 [params (def-params (first p))]
                                 [body (mk-Expr% (def-body (first p)))])
                            (get-loP (rest p)))]
    [(not (def? (first p))) (cons (mk-Expr% (first p)) 
                                  (get-loP (rest p)))]))

;;=============================================================================
; Testing
(begin-for-test
  (check-equal? (send (first 
                       (send (mk-Program% 
                              (list 
                               (make-arith 
                                '+ (list 
                                    2 
                                    (make-arith 
                                     '* (list 2 (make-arith '/ '(10 2))))))))
                             eval)) to-expr)
                12
                "Mixed Arith")
  (check-equal? (send (first 
                       (send (mk-Program% 
                              (list 
                               (make-bool 
                                'and 
                                (list #true 
                                      (make-bool 'or (list #true #false))))))
                             eval)) to-expr)
                #t
                "Mixed Bool")
  (check-equal? (send (first 
                       (send (mk-Program% 
                              (list 
                               (make-cmp '> (list 
                                             (make-arith '+ '(3 2 1)) 3 2 1))))
                             eval)) to-expr)
                #t
                "Mixed Cmp")
  (check-equal? (send (first 
                       (send (mk-Program% 
                              (list (make-if-exp
                                     (make-cmp '= '(3 3)) 
                                     (make-arith '- '(4 3)) #false)))
                             eval)) to-expr)
                1
                "Mixed if-exp")
  (check-equal? (send (first 
                       (send (mk-Program% 
                              (list 'f (make-def 'f '(x) 5)))
                             eval)) to-expr)
                (make-lam '(x) 5)
                "Var")
  (check-equal? (send (first 
                       (send (mk-Program% (list (make-arith '+ '(#true f))
                                                (make-def 'f 'x 5))) 
                             eval)) to-expr)
                INVALID_ARITH_ARGUMENTS
                "Invalid argument for Arithmetic operation")
  (check-equal? (send (first 
                       (send (mk-Program% (list (make-arith '+ '(3 f)))) 
                             eval)) to-expr)
                UNDEFINED_FUNCTION
                "Undefined function")
  (check-equal? (first (send (mk-Program% '()) eval)) 
                '()
                "Empty input list")
  (check-equal? (send (first (send (mk-Program% (list "ERR")) 
                                   eval)) to-expr)
                "ERR"
                "ErrString input")
  
  (check-equal? (send 
                 (send (mk-Expr% 
                        (make-lam 
                         '(z) (make-arith 
                               '+ (list 'x (make-if-exp 
                                            (make-cmp 
                                             '= (list 'x 4)) 
                                            (make-arith 
                                             '+ (list 'y 5 "error")) 
                                            (make-lam 
                                             '(x) (make-bool 
                                                   'and (list 4 'y #true))))))))
                       subst (new Number% [val 4]) 'x) to-expr)
                (make-lam
                 (list 'z)
                 (make-arith
                  '+
                  (list
                   4
                   (make-if-exp
                    (make-cmp '= (list 4 4))
                    (make-arith '+ (list 'y 5 "error"))
                    (make-lam (list 'x) (make-bool 'and (list 4 'y true)))))))
                "Subst lambda")
  
  (check-equal? (send (mk-Expr% #t) expr=? (mk-Expr% #t))
                #t
                "Expr=? Boolean")  
  (check-equal? (send (mk-Expr% 2) expr=? (mk-Expr% 2))
                #t
                "Expr=? Number")  
  (check-equal? (get-field val (send (mk-Expr% #t) subst (mk-Expr% 2) 'x))
                #t
                "Subst Boolean")  
  (check-equal? (get-field val (send (mk-Expr% 2) subst (mk-Expr% 2) 'x))
                2
                "Subst Number")
  (check-equal? (send (send (mk-Expr% 
                             (make-lam '(z) (make-bool 'and (list 4 'y #true))))
                            subst (new Number% [val 4]) 'x) to-expr)
                (lam '(z) (bool 'and '(4 y #t)))
                "Subst Bool"))


; exp/no-var test cases
(begin-for-test
  (check-equal?
   (send (mk-Expr% (make-lam '(x) 'x)) to-expr/no-var)
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (send (mk-Expr% (make-lam '(x y) (make-lam '(z) 
                                              (make-call 'x '(y z))))) 
         to-expr/no-var)
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (send (mk-Expr% "expr-string") to-expr/no-var)
   "expr-string"
   "returns string for expr e as String")
  (check-equal?
   (send (new CmpExpNoVar%) compare (make-arith '+ '(2 3)) 
         (make-arith '+ '(2 3 4)))
   #f
   "False on comparing unequal length args"))

; test cases
(begin-for-test
  (check-false
   (send (mk-Expr% (make-lam '(x y) (make-call 'x '(y)))) expr=? 
         (mk-Expr% (make-lam '(y x) (make-call 'x '(y)))))
   "not equivalent")
  (check-false
   (send (mk-Expr% "String1") expr=? 
         (mk-Expr% "String2"))
   "not equivalent")
  (check-equal?
   (send (mk-Expr% "expr-string") to-expr/no-var)
   "expr-string"
   "returns string for expr e as String")
  (check-equal?
   (send (mk-Expr% (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))) expr=? 
         (mk-Expr% (make-lam '(x) (make-lam '(y) (make-call 'x '(y))))))
   #t
   "equivalent nested-lambdas")
  (check-false
   (send (mk-Expr% (make-lam '(x) 'x)) expr=? 
         (mk-Expr% (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))))
   "not equivalent lambda's")
  (check-false
   (send (mk-Expr% (make-lam '(x) 'x)) expr=? 
         (mk-Expr% "expr-string"))
   "not equivalent expressions types.")
  (check-false
   (send (mk-Expr% (make-arith '- '(2 3))) expr=? 
         (mk-Expr% (make-bool 'and '(#false #true))))
   "not equivalent expressions types.")
  (check-false
   (send (mk-Expr% (make-bool 'or '(#false #true))) expr=? 
         (mk-Expr% (make-arith '+ '(4 3))))
   "not equivalent expressions types.")
  (check-false
   (send (mk-Expr% (make-cmp '< '(3 2))) expr=? 
         (mk-Expr% (make-arith '+ '(4 3))))
   "not equivalent expressions types.")
  (check-false
   (send (mk-Expr% (make-if-exp #true (make-lam '(x) 'x) (make-lam '(x) 'x))) 
         expr=? (mk-Expr% (make-arith '+ '(4 3))))
   "not equivalent expressions types.")
  (check-false
   (send (mk-Expr% (make-arith '- '(2 3))) expr=? 
         (mk-Expr% (make-arith '+ '(4))))
   "not equivalent symbols.")
  (check-false
   (send (mk-Expr% (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))) expr=? 
         (mk-Expr% (make-lam '(x) (make-lam '(y) (make-arith '+ '(2 3))))))
   "not equivalent  expressions types.")
  (check-false
   (send (mk-Expr% (make-arith '- '(2 3 4))) expr=? 
         (mk-Expr% (make-lam '(x) (make-lam '(y) (make-arith '- '(2 3))))))
   "not equivalent expressions types.")
  (check-equal?
   (send (mk-Expr% (make-arith '- '(2 3 4))) expr=? 
         (mk-Expr% (make-arith '- '(2 3 4))))
   #t
   "Equal arith expr")
  (check-equal?
   (send (mk-Expr% (make-if-exp #true (make-lam '(x) 'x) (make-lam '(x) 'x))) 
         expr=? 
         (mk-Expr% (make-if-exp #true (make-lam '(x) 'x) (make-lam '(x) 'x))))
   #t
   "Equal if-exp expr")
  (check-equal?
   (send (mk-Expr% (make-bool 'or '(#false #true))) 
         expr=? (mk-Expr% (make-bool 'or '(#false #true))))
   #t
   "Equal bool expr")
  (check-equal?
   (send (mk-Expr% (make-cmp '< '(3 2))) expr=? (mk-Expr% (make-cmp '< '(3 2))))
   #t
   "Equal cmp expr")
  (check-equal?
   (send (send (mk-Expr% (make-call 'x '(y))) eval 
               (list (new Def% [name 'x] [params '(y z)] 
                          [body (mk-Expr% 3)]))) to-expr)
   "Invalid number of arguments"
   "invalid call expr")
  (check-equal?
   (send (send (mk-Expr% (if-exp (make-arith '+ '(2 3)) '- '(#f)))
               eval '()) to-expr)
   "Invalid arguments for If-expression"
   "invalid if-exp expr")
  (check-equal?
   (send (send (mk-Expr% (make-cmp '< '(3 2))) eval '()) to-expr)
   #f
   "Cmp operation")
  (check-equal?
   (send (send (mk-Expr% (make-cmp '< '(3 x))) eval '()) to-expr)
   "Undefined function"
   "Undefined function")
  (check-equal?
   (send (send (mk-Expr% (make-cmp '< '(3 #t))) eval '()) to-expr)
   "Invalid arguments for Comparator"
   "Invalid arguments for Comparator")
  (check-equal?
   (send (send (mk-Expr% (make-bool 'and '(#t x))) eval '()) to-expr)
   "Undefined function"
   "Undefined function for Bool")
  (check-equal?
   (send (send (mk-Expr% (make-bool 'and '(#t 3))) eval '()) to-expr)
   "Invalid arguments for Boolean"
   "Invalid arguments for Boolean")
  (check-equal?
   (send (mk-Expr% (make-call 'y '(x))) to-expr)
   (call 'y '(x))
   "Call non object expr conversion")
  (check-equal?
   (get-field val (send (mk-Expr% #t) app '() '()))
   "Apply error - not lambda"
   "Apply on boolean")
  (check-equal?
   (get-field val (send (mk-Expr% "Err") app '() '()))
   "Apply error - not lambda"
   "Apply on string"))

(begin-for-test
  (check-equal?
   (send (mk-Expr% (make-arith '+ (list 2 (make-lam '(x) 'x)))) expr=? 
         (mk-Expr% (make-arith '+ (list 2 (make-lam '(y) 'y)))))
   #t
   "nested aithmetic lambdas")
  (check-equal?
   (send (mk-Expr% (make-bool 'and (list 2 (make-lam '(x) 'x)))) expr=?
         (mk-Expr% (make-bool 'and (list 2 (make-lam '(y) 'y)))))
   #t
   "nested bool lambdas")
  (check-equal?
   (send (mk-Expr% (make-cmp '< (list 2 (make-lam '(x) 'x)))) expr=? 
         (mk-Expr% (make-cmp '< (list 2 (make-lam '(y) 'y)))))
   #t
   "nested cmp lambdas")
  (check-equal?
   (send (mk-Expr% (make-if-exp #true (make-lam '(x) 'x) (make-lam '(x) 'x))) 
         expr=? 
         (mk-Expr% (make-if-exp #true (make-lam '(x) 'x) (make-lam '(y) 'y))))
   #t
   "nested if lambdas")
  (check-equal?
   (send (send (mk-Expr% (make-call (make-arith '+ '(2 3 4)) '(2 3))) eval '())
         to-expr)
   "Apply error - not lambda"
   "Call err")
  (check-equal?
   (send (send (mk-Expr% 'x) set-loSI 
               (list (new StaticInfo% [name 'x] [depth 0] [index 0]))) expr=? 
         (send (mk-Expr% 'x) set-loSI 
               (list (new StaticInfo% [name 'x] [depth 0] [index 0]))))
   #t
   "Var expr=?")
  (check-equal?
   (send (send (mk-Expr% (make-call 'x '(3))) set-loSI 
               (list (new StaticInfo% [name 'x] [depth 0] [index 0]))) 
         expr=? (send (mk-Expr% (make-call 'x '(3))) set-loSI
                      (list (new StaticInfo% [name 'x] [depth 0] [index 0]))))
   #t
   "Call expr=?"))

; Call test cases
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

(check-equal?
 (map (λ (x) (send x to-expr)) 
      (send (mk-Program% (list
                          MK-ADD-DEF
                          ADD5-DEF
                          ADD5-OR-6-DEF
                          (make-call 'add5 '(10))
                          (make-call 'add5-or-6 '(200))
                          (make-call 'add5-or-6 '(-100)))) eval))
 (list 15 205 -94)
 "call-fn evaluates to a function")