#lang racket


; **************************************** Fundament Wrapper ****************************************

; Define the modify! macro to overwrite variable definitions. Works with Higher Order Functions. 
(define-syntax-rule (modify! var val)
  (begin
    (set! var val)
    var))

; Define the assign macro to overwrite variable definitions. Does not work with Higher Order Functions. 
(define-syntax assign
  (syntax-rules ()
    ((_ val to var)
     (begin
       (set! var val)
       var))))

;(add 4 5)

;(define x 4)
;(modify! x (add x 3))
;7

; Add Macro. Works with numbers and variables equal to numbers
(define-syntax add
  (syntax-rules ()
    ((_ x y)
     (cond
       [(and (number? x) (number? y)) (+ x y)]))))

; Add Macro. Works with numbers and variables equal to numbers
(define-syntax subtract
    (syntax-rules ()
    ((_ x y)
     (cond
       [(and (number? x) (number? y)) (- x y)]))))

; Multiply Macro. Works with numbers and variables equal to numbers
(define-syntax multiply
    (syntax-rules ()
    ((_ x y)
     (cond
       [(and (number? x) (number? y)) (* x y)]))))

; Divide Macro. Works with numbers and variables equal to numbers
(define-syntax divide
  (syntax-rules ()
    ((_ x y)
     (cond
       [(eq? y 0) (error 'denom-equals-zero "Can't do that chief")]        
       [(and (number? x) (number? y)) (/ x y)]))))

; Print Macro. Works with strings or characters
(define-syntax print
  (syntax-rules ()
    ((_ query) (displayln query))))

; Uppercase Macro. Works with strings or characters
(define-syntax upper
  (syntax-rules ()
    ((_ query) (string-upcase query))))

; Lowercase Macro. Works with strings or characters
(define-syntax lower
  (syntax-rules ()
    ((_ query) (string-downcase query))))

; Exponent Macro. Works with numbers and variables equal to numbers
(define-syntax exp
  (syntax-rules ()
    ((_ x y) (expt x y))))

; Root Macro. Works with numbers and variables equal to numbers
(define-syntax root
  (syntax-rules ()
    ((_ x) (sqrt x))
    ((_ x y) (expt x (/ 1 y)))))

; Remainder Macro. Works with numbers and variables equal to numbers
(define-syntax rem
  (syntax-rules ()
    ((_ x y)
     (remainder x y))))

; Modulo Macro. Works with numbers and variables equal to numbers
(define-syntax mod
  (syntax-rules ()
    ((_ x y)
     (modulo x y))))

  

; **************************************** Control Stuctures ****************************************

; (define lst '(1 2 3 4 5 6))
; (for i in lst (displayln i))

; (for index in range 0 10 2 (modify! x (add x index)))
(define-syntax for
  (syntax-rules ()
    ; For Loop: Iterating between two numeric values
    ((_ var in range start stop step)
     (for-each (lambda (var) (printf "~s " var))
               (range start stop step)))

    ; For Loop: Iterating between two numeric values and higher order function functionality
    ((_ var in range start stop step bodyE ...)
     (let loop ((var start))
       (cond
         [(and (< var stop) (> step 0)) 
           (begin
             bodyE ...
             (loop (+ var step)))]
         [(and (> var stop) (< step 0)) 
           (begin
             bodyE ...
             (loop (- var (* step -1))))]
         [(and (> start stop) (> step 0))
          (error 'for-i-in-range "Start value is greater than the stop value, step failed")]
         [(and (< start stop) (< step 0))
          (error 'for-i-in-range "Start value is less than the stop value, step failed")])))

    ; For Loop: Iterating through a list with higher order functions 
    ((_ var in body bodyE)
     (let loop ((body body) (var 0))
       (cond
         [(not (null? body))
          (set! var (first body))
           (begin
             bodyE (first body) var
             (loop (rest body) var))])))))

#|
(define lst1 '(6 7 8))
(define lst2 '(a b c))
(define lst3 '(d e f))

(zip lst1 lst2 lst3)
|#

; Zip: Iterator of tuples where the first item in each passed iterator is paired together
(define (zip . lists)
  (define (loop listOflists result)
    (if (null? (first listOflists))
        (reverse result)
        (loop (map rest listOflists) (cons (map first listOflists) result))))
  (loop lists '()))

#|
(define lst3 '(d e f))
(enumerate lst3)

(cons (cons )) for dot pair
(cons (list )) for list pair
|#

; Enumerate: Iterator of tuple where the counter variable in each passed iterator
;   is paired together with the element at the current index in the list
(define (enumerate lst)
  (define (loop index lst result)
    (if (null? lst)
        (reverse result)
        (loop (+ index 1) (rest lst) (cons (list index (first lst)) result))))
  (loop 0 lst '()))

#|
(define x 0)
(while (< x 10)
  (displayln x)
  (set! x (+ x 1)))
|#

; While: Loop through logic until the inital condition is met
(define-syntax while
  (syntax-rules ()
    [(while test expr ...)
     (let loop ()
       (when test
         expr ...
         (loop)))]))

