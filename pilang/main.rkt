; bilge kaan guneyli
; 2020400051
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here
; 1. empty-state (5 points)
(define empty-state (hash))

; 2. get (5 points)
(define (get state var)
  (define my-hash state)
  (if (hash-has-key? my-hash var)
      (hash-ref my-hash var)
      (eval var)
  )
)

; 3. put (5 points)
(define (put state var val)
  (define my-hash state)
  (hash-set my-hash var val)
)

; 4. := (15 points)
(define (:= var val-expr state)
  (put (eval-expr val-expr state) var (get (eval-expr val-expr state) '-r))
)

; 5. if: (15 points)
(define (if: test-expr then-exprs else-exprs state)
  (if (get (eval-expr test-expr state) '-r)
      (eval-exprs then-exprs state)
      (eval-exprs else-exprs state)
  )
)

; 6. while: (15 points)
(define (while: test-expr body-exprs state)
  (if (get (eval-expr test-expr state) '-r)
     (while: test-expr body-exprs (eval-exprs body-exprs state))
     (eval-expr test-expr state)
  )
)

; 7. func (15 points)
(define (func params body-exprs state)
  (put state '-r (lambda args (get (eval-exprs body-exprs (add-to-hash state (create-hash params args)))'-r))) ;params'ı bir şekilde state'in içine atmak lazım 
)

(define (error-proof-eval car-lst state)
  (if (hash-has-key? state car-lst)
      (get state car-lst)
      (eval car-lst)
  )
)
(define (evaluate state lst)
  (define op (error-proof-eval (car lst) state))
  (define new-lst (map (curry numberize-item state) (cdr lst)))
  ;(if (procedure? op)
      (apply op new-lst)
      ;(get state op)
      ;) ;piazzadali dönüş evet olursa diye bir if 
)

(define (numberize-item state var)
  (if (hash-has-key? state var)
      (get state var)
      (get (eval-expr var state) '-r)
  )
)

(define (map-eval lst state)
  (map (lambda (expression) (evaluate state expression)) lst)
)

(define (list-expr expr state)
  (if (list? expr)
      expr
      (list expr)
  )
)

(define (contains-sublist? lst)
  (member #t (map list? lst))
  )


; 8. eval-expr (20 points)
(define (eval-expr expr state)
  (if (list? expr)
      (if (member (car expr) '(:= if: while: func)) ;liste ise
          (apply (eval(car expr)) (append (cdr expr)(list state)))
          (if (eq? (car expr) 'lambda) ;lambda ise
              (put state '-r (eval expr))
              (if (symbol? (car expr)) ;lambda değilse 
                  (put state '-r (car (map-eval (list (list-expr expr state)) state))) ;sembolse
                  (put state '-r expr)    
              )
          )
      )
      (if (hash-has-key? state expr) ;liste değilse
          (put state '-r (get state expr))
          (put state '-r (eval expr))
      )      
  )
)


; 9 eval-exprs (5 points)
(define (eval-exprs exprs state)
  (foldl eval-expr state exprs)
)
