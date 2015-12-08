;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xexpr) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require 2htdp/batch-io)
(require "extras.rkt")
(require rackunit)
(require racket/list)
(define TIME-ON-TASK 24)

(provide xexpr-element?)
(provide xexpr-tag)
(provide xexpr-attributes)
(provide xexpr-content)
(provide attribute-value)
(provide get-value)
(provide xexpr-find)
(provide xexpr-depth)
(provide xexpr-elements)  

; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
; Represents an XML element.

; TEMPLATE:
; xexpr-fn: Xexpr -> ??? 
; INTERP: The template describes the possible clauses that need be considered in
; a function that takes Xexpr as an input
; (define (xexpr-fn xe)
;  (cond
;    [(symbol? xe) ...]
;    [(string? xe) ...]
;    [(number? xe) ...]
;    [(list? xe) (cond 
;                  [(empty? xe) ...]
;                  [(symbol? (first xe))...
;                       (optional-loa+content (rest xe))...)]))
;
; optional-loa+content: [List-of Xexpr] -> ???
; INTERP: This is a template of a function that would be called if the input 
; is a list of Xexpr and list tasks need be performed on it. 
; Here in this template the remaining two definitions/possible variations of 
; the Xexpr expression template are handled
; (define (optional-loa+content x)
;  (cond
;    [(empty? optional-loa+content) ...]
;    [else ... (if (is-attribute-list? (first x)) ... (rest x)...x...)]))
;
; is-attribute-list?: [List-of Xexpr] or [List-of Attribute] -> Boolean
; Thsi function checks if the list passed to it is a list of attributes or not
; (define (is-attribute-list? y)
;  (if (list? y)
;      (cond
;        [(empty? y) ...]
;        [else
;         (local
;           (
;            (define F-y (first y)))
;           (cond
;             [(list? F-y)
;              (and (and (= (length F-y) 2) 
;                        (symbol? (first F-y)) 
;                        (string? (second F-y)))
;                   (list-of-attributes? (rest y)))]
;             [else ...]))])
;      ...))

; DATA EXAMPLES
(define HTML-EMPTY '(html ((lang "en-US")) (body (p) (br) (p) (br))))
(define HTML-WITH-TEXT
  '(html
    ((lang "en-US"))
    (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " (b "data") " definition."))))
(define IMG '(img ((src "balls.png") (width "100") (height "50"))))

; A Tag is a Symbol, representing the name of an XML element.

; DATA EXAMPLES
(define H1 'h1)
(define P 'p)
(define BR 'br)

; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.

; DATA EXAMPLES
(define HREF-NEU (list 'href "http://northeastern.edu"))
(define IMG-SRC (list 'src "ball.gif"))

; DATA EXAMPLES
; An AttrName is a Symbol, 
; representing the name of an XML attribute.
; An AttrValue is a String, 
; representing the value associated with an XML attribute.
(define GOOGLE (read-xexpr/web "http://www.google.com"))

(define CIRCLE (circle 40 "solid" "orange"))

; xexpr-element? : Any -> Boolean
; Returns true if x is a valid XML element.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-element? HTML-WITH-TEXT)
                #true
                "Function failed")
  (check-equal? (xexpr-element? HTML-EMPTY)
                #true
                "Function failed")
  (check-equal? (xexpr-element? IMG)
                #true
                "Function failed")
  (check-equal? (xexpr-element? 'html)
                #true
                "Function failed")
  (check-equal? (xexpr-element? 3)
                #true
                "Function failed")
  (check-equal? (xexpr-element? "test")
                #true
                "Function failed")
  (check-equal? (xexpr-element? '())
                #false
                "Function failed")
  (check-equal? (xexpr-element? GOOGLE)
                #true
                "Function failed"))
; STRATEGY: Data Decomposition on xe : Xexpr
(define (xexpr-element? xe)
  (cond
    [(symbol? xe) #true]
    [(string? xe) #true]
    [(number? xe) #true]
    [(list? xe) (if (empty? xe) 
                    #false 
                    (is-xexpr? xe))]))

; is-xexpr? : [List-of Any] -> Boolean
; Checks whether the input list is a [List-of Xexpr]
; STRATEGY: Data Decomposition on xe : Xexpr
(define (is-xexpr? xe)
  (if (symbol? (first xe)) (check-rest? (rest xe)) (xexpr-element? (first xe))))

; check-rest?: [List-of Any] -> Boolean
; Checks whether the input list is a [List-of Attribute] with 
; [List-of Xexpr] or only a valid [List-of Xexpr]
; STRATEGY: Data Decomposition on xe : Xexpr
(define (check-rest? xe)
  (cond 
    [(empty? xe) #false]
    [else (or (and (list-of-attributes? (first xe)) (empty? (rest xe)))
              (and (list-of-attributes? (first xe)) (xexpr-element? (rest xe)))
              (xexpr-element? xe))]))

; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-tag HTML-WITH-TEXT)
                'html
                "Function failed")
  (check-equal? (xexpr-tag "test")
                #false
                "Function failed")
  (check-equal? (xexpr-tag IMG)
                'img
                "Function failed")
  (check-equal? (xexpr-tag 'body)
                'body
                "Function failed")
  (check-equal? (xexpr-tag 3)
                #false
                "Function failed")
  (check-equal? (xexpr-tag '())
                #false
                "Function Failed")
  (check-equal? (xexpr-tag '("test" img))
                #false
                "Function failed"))
; STRATEGY: Data Decomposition on xe: Xexpr
(define (xexpr-tag xe)
  (cond
    [(symbol? xe) xe]
    [(string? xe) #false]
    [(number? xe) #false]
    [(list? xe) 
     (cond
       [(empty? xe) false]
       [else
        (if (symbol? (first xe)) 
            (first xe) 
            #false)])]))

; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-attributes HTML-WITH-TEXT)
                '((lang "en-US"))
                "Function failed")
  (check-equal? (xexpr-attributes HTML-EMPTY)
                '((lang "en-US"))
                "Function failed")
  (check-equal? (xexpr-attributes IMG)
                '((src "balls.png") (width "100") (height "50"))
                "Function failed")
  (check-equal? (xexpr-attributes 'body)
                '()
                "Function failed")
  (check-equal? (xexpr-attributes 3)
                '()
                "Function failed")
  (check-equal? (xexpr-attributes '())
                '()
                "Function Failed")
  (check-equal? (xexpr-attributes '("test" img))
                '()
                "Function failed")
  (check-equal? (xexpr-attributes "test")
                '()
                "function failed")
  (check-equal? (xexpr-attributes '(body (p "Here is assignment " 5)))
                '()
                "Function failed")
  (check-equal? (list-of-attributes? '(html ((lang "en-US" "en-GB")) 
                                            (p ("test"))))
                #false
                "Function failed"))
; STRATEGY: Data Decomposition on xe : Xexpr
(define (xexpr-attributes xe)
  (cond
    [(symbol? xe) '()]
    [(string? xe) '()]
    [(number? xe) '()]
    [(list? xe) (if (> (length xe) 1) 
                    (get-attributes (rest xe)) 
                    '())]))

; get-attributes: [List-of Attribute] or [List-of Xexpr] -> [List-of Attribute]
; retrieves the list of attributes from an Xexpr if it has any
; STRATEGY: Data Decomposition on xe : [List-of Attribute] or [List-of Xexpr]
(define (get-attributes xe)
  (if (list-of-attributes? (first xe)) 
      (first xe) 
      '()))

; list-of-attributes?: [List-of Attribute] or [List-of Xexpr] -> Boolean
; is the given value a list of attributes?
; STRATEGY: Data Decomposition on x : [List-of Attribute] or [List-of Xexpr]
(define (list-of-attributes? x)
  (if (list? x)
      (cond
        [(empty? x) #true]
        [else
         (local
           (
            (define F-x (first x)))
           (cond
             [(list? F-x)
              (and (and (= (length F-x) 2) 
                        (symbol? (first F-x)) 
                        (string? (second F-x)))
                   (list-of-attributes? (rest x)))]
             [else #false]))])
      #false))

; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-content HTML-WITH-TEXT)
                '((body
                   (p "Here is assignment " 5)
                   (br)
                   (p "This is a " (b "data") " definition.")))
                "Function failed")
  (check-equal? (xexpr-content HTML-EMPTY)
                '((body (p) (br) (p) (br)))
                "Function failed")
  (check-equal? (xexpr-content IMG)
                '()
                "Function failed")
  (check-equal? (xexpr-content 'body)
                '()
                "Function failed")
  (check-equal? (xexpr-content 3)
                '()
                "Function failed")
  (check-equal? (xexpr-content '())
                '()
                "Function Failed")
  (check-equal? (xexpr-content '(p "Here is assignment " 5))
                '("Here is assignment " 5)
                "Function failed"))                
; STRATEGY : Data Decomposition xe : Xexpr
(define (xexpr-content xe)
  (cond
    [(symbol? xe) '()]
    [(string? xe) '()]
    [(number? xe) '()]
    [(list? xe) (if (not (empty? xe)) (get-content (rest xe)) '())]))

; get-content: [List-of Xexpr] -> [List-of Xexpr]
; gets the content of the element 
; STRATEGY: Data Decompsition on x : [List-of Xexpr]
(define (get-content x)
  (cond
    [(empty? x) '()]
    [else (if (list-of-attributes? (first x))
              (rest x)
              x)]))

; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of first attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (attribute-value '((src "balls.png") 
                                   (width "100") (height "50")) 'width)
                "100"
                "Function failed")
  (check-equal? (attribute-value '((src "balls.png") 
                                   (width "100") (height "50")) 'html)
                #f
                "Function failed")
  (check-equal? (attribute-value '() 'width)
                #f
                "Function failed"))
; STRATEGY: Data Decomposition on  loa : ListOf<Attribute>
(define (attribute-value loa aname)
  (cond
    [(empty? loa) #f]
    [(false? (member? aname (map first loa))) #f]
    [else
     (second (first
              (filter (λ(l) (symbol=? aname (first l))) loa)))]))

; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (get-value HTML-EMPTY 'width)
                #f
                "Function failed")
  (check-equal? (get-value HTML-WITH-TEXT 'lang)
                "en-US"
                "Function failed")
  (check-equal? (get-value IMG 'src)
                "balls.png"
                "Function failed"))
; STRATEGY : Function composition
(define (get-value xe aname) 
  (attribute-value (xexpr-attributes xe) aname))

; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-find HTML-EMPTY 'width)
                '()
                "Function failed")
  (check-equal? (xexpr-find HTML-WITH-TEXT 'lang)
                '("en-US")
                "Function failed")
  (check-equal? (xexpr-find IMG 'src)
                '("balls.png")
                "Function failed")
  (check-equal? (xexpr-find IMG 'src)
                '("balls.png")
                "Function failed")
  (check-equal? (xexpr-find  '(html
                               ((lang "en-US"))
                               (body ((lang "en-GB"))
                                     (p "Here is assignment " 5)
                                     (br)
                                     (p "This is a " 
                                        (b "data") 
                                        " definition.")))
                             'lang)
                '("en-US" "en-GB")
                "Function failed"))
; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-find xe aname)
  (map second
       (filter (λ(x) (equal? (first x) aname)) (get-attributes-list xe))))

; get-attributes-list: Xexpr -> [List-of Attributes]
; Gets the list of all attributes of all elements including 
; nested elements within an Xexpr
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (get-attributes-list  '(html ((lang "en-US")) 
                                             (body ((lang "en-GB"))
                                                   (p "Here is assignment " 5) 
                                                   (br)
                                                   (p "This is a " 
                                                      (b "data") 
                                                      " definition."))))
                '((lang "en-US") (lang "en-GB"))
                "Function failed"))
; STRATEGY: Function Composition
(define (get-attributes-list xe)
  (foldr append '()
         (filter (λ(x) (not (empty? x)))
                 (map attribute-list xe))))

; attribute-list: Xexpr -> [List-of Attributes]
; Gets the list of all attributes of an element
; STRATEGY: Data Decomposition on xe: Xexpr
(define (attribute-list xe)
  (cond
    [(not (list? xe)) '()]
    [(list? xe)
     (cond
       [(empty? xe) '()]
       [(list-of-attributes? xe) xe]
       [else (append
              (extract-further (first xe))
              (attribute-list (rest xe)))])]))

; extract-further: Xexpr -> [List-of Attributes]
; Gets the list of all attributes of only nested elements of an Xexpr
; STRATEGY: Function Composition
(define (extract-further xe)
  (if (list? xe)
      (cond
        [(list-of-attributes? xe) xe]
        [(> (length xe) 2)
         (attribute-list xe)]
        [(<= (length xe) 3)
         (xexpr-attributes xe)])
      '()))

; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-depth HTML-EMPTY)
                2
                "Function failed")
  (check-equal? (xexpr-depth HTML-WITH-TEXT)
                4
                "Function failed")
  (check-equal? (xexpr-depth IMG)
                0
                "Function failed")
  (check-equal? (xexpr-depth 'html)
                0
                "Function failed"))
; STRATEGY : Function composition
(define (xexpr-depth xe)
  (cond
    [(not (list? xe)) 0]
    [(and (= (length xe) 2)
         (list-of-attributes? (second xe))) 0]
    [else (add1
           (foldr max 0 (map find-element-depth xe)))]))

; find-element-depth: Xexpr -> Natural
; Delves one step ahead into the content of the input element
; STRATEGY: Data decomposition on xe: Xexpr
(define (find-element-depth xe)
  (cond
    [(symbol? xe) 0]
    [else
     (cond
       [(list-of-attributes? xe) 0]
       [else (+ 
              (if (symbol? (first xe))
                  1
                  (depth-further (first xe)))
              (find-element-depth (rest xe)))])]))

; depth-further: Xexpr -> Natural
; Delves deeper into the nested elements of the Xexpr and calculates depth
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (depth-further '((src "balls.png") (width "100") (height "50")))
                0
                "Function failed"))
; STRATEGY : Function composition
(define (depth-further xe) 
  (if (list? xe)
      (cond
        [(list-of-attributes? xe) 0]
        [(> (length xe) 3)
         (add1 (find-element-depth xe))]
        [else 0])
      0))

; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
; FUNCTION EXAMPLES:
(begin-for-test
  (check-equal? (xexpr-elements HTML-WITH-TEXT P)
                '((p "Here is assignment " 5) 
                  (p "This is a " (b "data") " definition."))
                "Function failed")
  (check-equal? (xexpr-elements HTML-EMPTY 'body)
                '((body (p) (br) (p) (br)))
                "Function failed")
  (check-equal? (xexpr-elements IMG 'body)
                '()
                "Function failed")
  (check-equal? (xexpr-elements IMG 'src)
                '()
                "Function failed")
  (check-equal? (xexpr-elements IMG 'img)
                '((img ((src "balls.png") (width "100") (height "50"))))
                "Function failed"))
; STRATEGY: Function Composition
(define (xexpr-elements xe t)
  (first
   (remove-duplicates
    (map (λ(x) (xexpr-element-list xe t)) xe))))

; xexpr-element-list: Xexpr Tag -> ListOf<Xexpr>
; Gets a list of elements with the same tag
; STRATEGY: Data Decomposition on xe : Xexpr
(define (xexpr-element-list xe t)
  (cond
    [(not (list? xe)) '()]
    [(and (list? xe) (not (empty? xe)))
     (append
      (if (and (symbol? (first xe))
               (symbol=? (first xe) t))
          (list xe)
          (find-tags (first xe) t))
      (xexpr-element-list (rest xe) t))]
    [else '()]))

; find-tags : Xexpr Tag -> ListOf<Xexpr>
; Checks nested elements and gets a list of elements with the same tag
; STRATEGY : Data Decomposition on xe: Xexpr
(define (find-tags xe t)
  (if (list? xe)
      (cond
        [(list-of-attributes? xe) '()]
        [(> (length xe) 2)
         (xexpr-element-list xe t)]
        [else (if (symbol=? (first xe) t)
                  (list xe)
                  '())])
      '()))

; make-url: String-> String
; creates an xexpr representation of the url which takes
; name as an input 
; FUNCTION EXAMPLES:
(begin-for-test
  (check-not-equal? (make-url "apple")
                    (read-xexpr/web "http://www.google.com/finance?q=hp")
                    "Wrong URL!"))
; STRATEGY: Function composition                
(define (make-url name)
  (read-xexpr/web (string-append "http://www.google.com/finance?q=" name)))
; Test-suite
(begin-for-test
  (check-not-equal? (make-url "ford")
                    (read-xexpr/web "http://www.google.com/finance?q=fofrtrd")
                    "Wrong URL!"))

; crawl-data: String-> Maybe<Image>
; Retrieves the stock price of the given
; company 'name' and returns an image with the stock price
; rendered on it but returns false when error
; FUNCTION EXAMPLES: 
(begin-for-test
  (check-equal? (crawl-data "delld")
                #false
                "Invalid brand!"))
; STRATEGY: Function Composition
(define (crawl-data name)
  (local
    (
     (define STOCK-DATA 
       (filter
        (λ(x) (equal? (second x) (list (list 'class "pr"))))
        (xexpr-elements (make-url name) 'span)))
     (define STOCK-PRICE
       (if (empty? STOCK-DATA) #false
           (third
            (fourth
             (first STOCK-DATA))))))
    (if (false? STOCK-PRICE)
        #false
        (overlay
         (text STOCK-PRICE 20 "olive")
         CIRCLE))))
; Test-suite: 
(begin-for-test
  (check-equal? (crawl-data "hptgy")
                #false
                "Invalid brand!"))

;.................... Alternate Data Definitions........
; A)

; An Xexpr is one of:
; - Atom
; - XL
; 
; An Atom is one of :
; - Number
; - String
; - Symbol
;
; An XL is one of:
; (cons symbol (cons [List-of Attribute] [List-of Xexpr]))
; (cons symbol (con [List-of Xexpr]))

; Here Attribute still is a (list Symbol String)
; The Symbol represents Attribute Name and String represents Attribute Value

; The above data definition varies from the one currently used in this program 
; in separating the Atomic forms of the xexpr from the list form of the same.
; The checks on the atomic data could all be brought down to a single function 
; and used rather than repeating the checks for each through out the code..

; However in case of attribute lists we might want to check if the first 
; element in the list is a symbol or not, in which case the function would not 
; be of any use

; B)
; An Xexpr is one of:
; - Symbol
; - Xword
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)

; And here Xword is 
; A XWord is '(word ((text String))).

; The above alternate data definition allows us to represent the xexpr 
; (when it contains text in it) as a representation of text within enumeration