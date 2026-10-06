#lang racket/base

(provide environment
         environment-scope
         environment-parent
         resolve
         global-env)

(struct environment ([parent #:mutable] [scope #:mutable]) #:transparent)

(define global-env 
  (environment (void) 
               (make-hash (list (cons '+ +)
                                (cons '* *)
                                (cons '- -)
                                (cons '/ /)
                                (cons '> >)
                                (cons '>= >=)
                                (cons '< <)
                                (cons '<= <=)
                                (cons '= =)
                                ))))

; takes in an environment and searches each parent env until it finds
; the given id. Returns env that contains it.
(define (resolve env id)
  (cond
    [(hash-has-key? (environment-scope env) id) env]
    [(not (equal? (environment-parent env) (void))) (resolve (environment-parent env) id)]
    [else (error (format "~s not defined in scope" id))]))

