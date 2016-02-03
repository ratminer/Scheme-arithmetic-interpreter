#lang racket
; Robert Fernandes
; SYSC3101 Arithmetic interpreter
; 100887093
;
; <expression> ::= <number> | "(" <operator> <expression> <expression> ")"
; <operator> ::= "+" | "-" | "*" | "/"

(define (make-stack)
  (let ((stack '()))
    (lambda (msg . args)
      (cond 
        [(eq? msg 'pop!)  (set! stack (cdr stack))]
        [(eq? msg 'push!) (set! stack (append (reverse args) stack))]
        [(eq? msg 'stack) stack]
        [else "Not valid message!"]))))

(define expression (make-stack))
(define operators '(+ - * /))

(define (add-arg expr)
  (cond ;; self-evaluating object?  (we only handle numbers)
        [(number? expr)
         (expression 'push! expr)]
        ;; compound expression? (we only handle two-arg combinations)
        [(list? expr)
         (eval-expr expr)]
        [else
         (error "Invalid argument:" expr)]))

(define (eval-expr1 expr)
  (let ((operator-name (car expr)))
    (expression 'push! operator-name))
  (let ((arg1 (add-arg (caddr expr)))
        (arg2 (add-arg (cadr expr))))
    (printf "")
    ))

(define (eval-expr expr)
  (cond [(not(eqv? 3 (length expr)))
         (error "Invalid number of arguments in expr:" expr)]
        [(not(member (car expr) operators))
         (error "Invalid operation in expr:" expr)])
  (let ((operator-name (car expr)))
    (expression 'push! operator-name))
  (add-arg (caddr expr))
  (add-arg (cadr expr)))

(define (out)
  (let ((stack (make-stack)))
    (for ([i (expression 'stack)])
      (cond [(number? i)
           (printf "move ~a into register-~a\n" i (add1(length (stack 'stack))))
           (stack 'push! i)]
          [(member i operators)
           (print-op i)
           (printf "register-~a and register-~a\n" (sub1(length (stack 'stack)))(length(stack 'stack)))
           (stack 'pop!)
           (stack 'pop!)
           (stack 'push! 'x)]
          )
      (expression 'pop!)
      )
    )
  )

(define (print-op x)
  (cond ((eq? x '+)
            (printf "add "))
           ((eq? x '-)
            (printf "subtract "))
           ((eq? x '*)
            (printf "multiply "))
           ((eq? x '/)
            (printf "divide "))))

(define (compile)
  (define expr (read))
  (add-arg expr)
  (out))