#lang racket/base

(require rackunit
         "environment.rkt"
         "main.rkt")

; math operations
(check-eq? (eval "Hello") "Hello" "strings")
(check-eq? (eval 4) 4 "numbers")
(check-eq? (eval '(+ 3 2)) 5 "addition")
(check-eq? (eval '(+ (+ 3 4) 2)) 9 "one nested addition")
(check-eq? (eval '(+ (+ 3 4) (+ 10 9))) 26 "two nested additions")

; variables
(check-eq? (eval '(var x (+ 3 4))) 7 "var assignment")
(check-eq? (eval '(+ x 7)) 14 "var lookup")

; blocks
(check-eq? (eval '(begin (+ 2 3))) 5 "block single scope chain")
(check-eq? (eval '(begin (var x (+ 2 3)) (+ x 1))) 6 "block single scope chain, multiple expressions")
(check-eq? (eval '(begin (var x (+ 2 3)) (begin (var y 10) y) (+ x 1))) 6 "block multiple scope chains")
(check-eq? (eval '(begin (var x (+ 2 3)) (begin (var x 10) x) (+ x 1))) 6 "block multiple scope chains, shadowed variable")
(check-eq? (eval '(begin (var x (+ 2 3)) (var result (begin (var y 10) (+ y x))) result)) 15 "block multiple scope chains, identifier resolution")
(check-eq? 
  (eval 
    '(begin 
       (var x (+ 2 3)) 
       (var result (begin (set x (+ 5 5)) 7)) 
       x))
  10
  "block multiple scope chains, identifier resolution with set")

; conditions
(check-eq? 
  (eval 
    '(if (> 2 1)
         (+ 3 1)
         5)) 4 "if condition equals consequent with >")
(check-eq? 
  (eval 
    '(if (<= 1 1)
         (+ 3 1)
         5)) 4 "if condition equals consquent with <=")
(check-eq? 
  (eval 
    '(if (= 1 1)
         (+ 3 1)
         5)) 4 "if condition equals consequent with =")

; loops
(check-eq?
  (eval 
    '(begin 
       (var i 1)
       (while (< i 5)
              (begin 
                (var x 3)
                (set i (+ i 1))
                (+ x i)))))
  8 "while loop")
(check-eq?
  (eval 
    '(begin 
       (var i 1)
       (while (< i 5)
              (begin 
                (set i (+ i 1))
                i))))
  5 "while loop mutation")
(check-eq?
  (eval 
    '(begin 
       (var i 1)
       (while (< i 0)
              (begin 
                (set i (+ i 1))
                i))))
  (void) "while loop void return")

; functions
(check-eq?
  (eval
    '(begin
       (var x 10)
       (def foo () x)

       (def bar ()
            (begin
              (var x 20)
              (+ x (foo))))

       (bar)))
  30 "functions lexical scope")
(check-eq?
  (eval
    '(begin
       (var z 100)

       (def foo (x y)
            (begin
              (var a (+ x y))
              (def inner (b)
                   (begin 
                     (var c (+ a b))
                     (+ c z)))

              inner))

       (var b (foo 10 20))
       (b 20)))
  150 "function closures with parameters")
(check-eq?
  (eval 
    '(begin
       ((lambda () (begin (* 2 2))))))
  4 "immediately invoked lambda expression")
(check-eq?
  (eval 
    '(begin
       ((lambda (x y) (begin (+ x y))) 10 10)))
  20 "immediately invoked lambda expression with arguments")
(check-eq?
  (eval 
    '(begin 
       (var x (lambda (x) (begin (* x x))))
       (x 2)))
  4 "lambda into variable")
(check-eq?
  (eval 
    '(begin 
       (var x (lambda (x) (* x x)))
       (x 2)))
  4 "lambda into variable single expression body")
(check-eq?
  (eval 
    '(begin 
       (def foo (f)
            (begin 
              (f 2 2)))

       (var z 10)
       (foo (lambda (x y) 
              (begin 
                (var a (+ x y))
                (+ a z))))))
  14 "higher order function with lambda")
(check-eq?
  (eval
    '(begin 
       (def factorial (x)
            (if (= x 0)
                1 
                (* x (factorial (- x 1)))))

       (factorial 5)))
  120 "factorial function")

; syntax sugar 
(check-eq? 
  (eval 
    '(begin 
       (var test 3)
       (++ test)))
    4 "++")
(check-equal?
  (eval 
    '(begin 
       (var x 10)
       (switch ((= x 10) 100)
               ((> x 10) 200)
               (else 0))))
  100 "switch")
(check-equal?
  (eval 
    '(begin 
       (var x 11)
       (switch ((= x 10) 100)
               ((> x 10) 200)
               (else 0))))
  200 "switch second case")
(check-equal?
  (eval 
    '(begin 
       (var x 5)
       (switch ((= x 10) 100)
               ((> x 10) 200)
               (else 0))))
  0 "switch else case")
(check-equal? 
  (eval 
    '(begin 
       (for (var x 10)
            (> x 0)
            (-- x)
            (* x 10))))
  0 "for loop")
(check-equal? 
  (eval 
    '(begin 
       (for (var x 0)
            (< x 10)
            (++ x)
            x)))
  (eval-global
     '(var x 0)
     '(while (< x 10)
            (begin
              (++ x))))
    "for loop equal to while loop")

; classes and objects
(check-eq? 
  (eval-global
    '(class Point null
       (begin 
         (def constructor (self x y)
              (begin 
                (set (prop self x) x)
                (set (prop self y) y)))

         (def calc (self)
              (+ (prop self x)
                 (prop self y)))))
    '(var p (new Point 10 20))
    '((prop p calc) p))
  30 "class constructor and method call")
  ; looks up class env from name
  ; creates an instance env from class env 
  ; evaluates the arguments and maps them to the parameters under the instace env; 'self' refers to the instance env which is looked up
  ; evaluates the body of the constructor (mapped in class env) under the instance env 

(check-eq? 
  (eval-global
    '(class Point null
       (begin 
         (def constructor (self x y)
              (begin 
                (set (prop self x) x)
                (set (prop self y) y)))

         (def calc (self)
              (+ (prop self x)
                 (prop self y)))))

    '(class Point3D Point
       (begin 
         (def constructor (self x y z)
              (begin 
                ((prop (super Point3D) constructor) self x y)
                (set (prop self z) z)))

         (def calc (self)
              (+ ((prop (super Point3D) calc) self)
                 (prop self z)))))

    '(var p (new Point3D 10 20 30))
    '((prop p calc) p))
  60 "class super invocation")
