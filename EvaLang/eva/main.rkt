#lang racket/base

(require "environment.rkt"
         racket/list
         racket/match)

(provide eval
         eval-global)

(struct closure (parameters body env) #:transparent)

(define (lookup name env)
     (hash-ref (environment-scope (resolve env name)) name))

(define (eval-global . expr) (eval (cons 'begin expr) global-env))

; TODO: make eval curried with env first to reduce redundant lambdas
(define (eval expr [env global-env])
  (cond
    [(number? expr) expr]
    [(string? expr) expr]
    [(symbol? expr) (lookup expr env)]

    [(equal? (car expr) 'var)
       (define initialized-expr (eval (caddr expr) env))

       (hash-set! (environment-scope env) (cadr expr) initialized-expr)
       initialized-expr]

      [(equal? (car expr) 'set)
       (cond 
         [(equal? (and (list? (second expr)) (car (second expr))) 'prop)
          (define inner-prop (second expr))
          (define-values (instance parameter-value) 
            (values (eval (second inner-prop) env) (eval (third inner-prop) env)))

          (hash-set! (environment-scope instance) (third inner-prop) parameter-value)]
         [else 
           (define initialized-expr (eval (third expr) env))
           (hash-set! (environment-scope (resolve env (cadr expr))) (second expr) initialized-expr)
           initialized-expr])]

      [(equal? (car expr) 'begin)
       (define exprs (cdr expr))
       (define new-env (environment env (make-hash)))

       (for/last ([e exprs]) (eval e new-env))]

      [(equal? (car expr) 'while)
       (let while ([result (void)])
         (if (eval (second expr) env)
             (while (eval (third expr) env))
             result))]

      [(equal? (car expr) 'if)
       (match-define (list condition consequent alternate) (cdr expr))
       (if (eval condition env) (eval consequent env) (eval alternate env))]

      [(equal? (car expr) 'def) ; just syntax sugar for (var name (lambda (x) (begin x)))
       (match-define (list name parameters body) (cdr expr))

       (define saved-lambda-node (list 'var name (list 'lambda parameters body))) ; JIT compile to saved lambda
       (eval saved-lambda-node env)]

      [(equal? (car expr) 'lambda)
       (match-define (list parameters body) (cdr expr))
       (closure parameters body env)]

      [(equal? (car expr) '++)
       (define var-name (second expr))
       (eval (list 'set var-name (list '+ var-name 1)) env)]

      [(equal? (car expr) '--)
       (define var-name (second expr))
       (eval (list 'set var-name (list '- var-name 1)) env)]

      [(equal? (car expr) 'switch)
       (eval 
         (let switch->ifs ([branches (cdr expr)])
           (define curr-branch (car branches))

           (cond 
             [(equal? (car curr-branch) 'else) (second curr-branch)]
             [else (list 'if (car curr-branch)
                             (cadr curr-branch)
                             (switch->ifs (cdr branches)))])) env)]

      [(equal? (car expr) 'for)
       (eval
         (list 'begin 
               (second expr) ; variable initializer
               (list 'while 
                     (third expr) ; condition
                     (list 'begin 
                           (fifth expr) ; body
                           (fourth expr)))) env)] ; variable modifier

      [(equal? (car expr) 'class)
       (define-values (name parent-class body)
                      (values (second expr) (third expr) (fourth expr)))
       (define class-env (environment 
                           (if (equal? parent-class 'null) env (lookup parent-class env))
                           (make-hash)))

       (for ([e (cdr body)]) (eval e class-env))
       (hash-set! (environment-scope env) name class-env)]

      [(equal? (car expr) 'new)
       (define-values (class-name arguments)
                      (values (second expr) (cddr expr)))

       (define class-env (lookup class-name env))
       (define instance-env (environment class-env (make-hash)))

       (define constructor-closure (lookup 'constructor class-env))
       
       (define ids (map cons (closure-parameters constructor-closure) 
                             (cons instance-env (map (lambda (x) (eval x env)) arguments))))

       (for ([id ids]) (hash-set! (environment-scope instance-env) (car id) (cdr id)))

       (eval (closure-body constructor-closure) instance-env)
       instance-env]

      [(equal? (car expr) 'prop)
       (define-values (instance-env-arg name)
                      (values (second expr) (third expr)))

       (define instance-env (eval instance-env-arg env))
       (lookup name instance-env)]

      ; (super ClassName) -> returns parent class env
      [(equal? (car expr) 'super)
       (define class-name (second expr))
       (environment-parent (eval class-name env))] 

      [(equal? (car expr) 'module)
       (define-values (module-name body)
         (values (second expr) (third expr)))

       (define module-env
         (environment env (make-hash)))
       (hash-set! (environment-scope env) module-name module-env)

       (for ([e (cdr body)]) (eval e module-env))

       module-env]

      [(list? expr)
       (define id-value (eval (car expr) env))

       (define (argument-values) 
         (map (lambda (x) (eval x env)) (cdr expr)))

       (cond 
         [(procedure? id-value) (apply id-value (argument-values))]
         [(closure? id-value)
          (define ids (map cons (closure-parameters id-value) (argument-values)))
          (define new-env (environment 
                            (closure-env id-value)
                            (make-hash ids)))

          (eval (closure-body id-value) new-env)]
         [else (raise "Symbol is not a procedure")])]

      [else (raise (format "~s expression not supported" expr) #t)]))


