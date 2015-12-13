(load "~/scheme/constraint.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;33
(define (averager a b c)
  (let ((sum (make-connector))
        (d (make-connector)))
    (adder a b sum)
    (multiplier sum d c)
    (constant (/ 1 2) d)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
  
(probe "a" a)
(probe "b" b)
(probe "(a+b)/2" c)
(averager a b c)
(set-value! a 2 'user)
(set-value! b 4 'user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me)
            (error "Neither a nor b has value"))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond
     ((eq? request 'I-have-a-value)
      (process-new-value))
     ((eq? request 'I-lost-my-value)
      (process-forget-value))
     (else
      (error "Unknown request -- MULTIPLIER" request))))
  (connect a me)
  (connect b me)
  me)

(define s (make-connector))
(probe "a^2" s)
(squarer a s)
(forget-value! a 'user)
(set-value! s 36 'user)

(forget-value! b 'user)
(set-value! b 12 'user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
