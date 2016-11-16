(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond
    ((null? items) nil)
    (else
      (cons
        (proc (car items))
        (map proc (cdr items))
      )
    )
  )
)

(define (cons-all first rests)
  (if (null? rests)
    ()
    (map
      (lambda (lst) (cons first lst))
      rests
    )
  )
)

(define (zip pairs)
  (if (null? pairs) '(() ())
    (cons (cons (caar pairs) (car (zip (cdr pairs)))) 
      (list (cons (car (cdar pairs)) (car (cdr (zip (cdr pairs))))))
      )
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
    (define (index n x)
      (if (null? x)
        ()
        (cons
          (list n (car x))
          (index (+ n 1) (cdr x))
        )
      )
    )
  (index 0 s)
)

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (cond
    ((or (equal? total 0) (null? denoms)) nil)
    ((> total (car denoms)) (append
      (cons-all
        (car denoms)
        (list-change (- total (car denoms)) denoms)
      )
      (list-change total (cdr denoms)))
    )
    ((< total (car denoms))
      (list-change total (cdr denoms))
      )
    (else
      (cons (list total) (list-change total (cdr denoms)))
    )
  )
)

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (let-to-lambda body)))
        ))
        ((let? expr)
         (let ((values (cadr expr))
               (body (cddr expr)))
           (cons
            (list 'lambda (let-to-lambda (car (zip values))) (let-to-lambda (car body)))
            (let-to-lambda (cadr (zip values)))
           )
        ))
        (else
          (map let-to-lambda expr)
        )
  )
)
