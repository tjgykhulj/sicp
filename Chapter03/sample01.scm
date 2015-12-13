;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define new-withdraw1
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient founds"))))

(define (new-withdraw2 amount)
  (define balance 100)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
                 balance)
      "Insufficient founds"))

;;;;;;;;;;;;;;;;;;;;The two function above are not the same

(define (make-account balance)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient founds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define w1 (make-account 100))
(define w2 (make-account 1000))
((w1 'withdraw) 20)
((w2 'withdraw) 20)
((w1 'deposit) 99)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))
;TEST
(define A (make-accumulator 5))
(A 10)
(A 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
(define (make-monitored func)
  (define times 0)
  (define (dispatch op)
    (cond ((number? op)
           (set! times (+ times 1))
           (func op))
          ((eq? op 'how-many-calls?) times)
          ((eq? op 'reset-count) (set! times 0) 0)
          (else
           (error "Unknown request -- MAKE_MONITORED" m))))
  dispatch)
;TEST
(define s (make-monitored sqrt))
(s 100)
(s 9801)
(s 'how-many-calls?)
(s 'reset-count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3\4
(define (make-account money password)

  (define wrong-times 0)
  (define (withdraw amount)
    (if (>= money amount)
        (begin (set! money (- money amount))
               money)
        "Insufficient founds"))
  
  (define (deposit amount)
    (set! money (+ money amount))
    money)

  (define call-the-cops "haha")
  (define (warning . a)
    (set! wrong-times (+ wrong-times 1))
    (if (= wrong-times 7)
        call-the-cops
        "Incorrect password"))

  (define (dispatch input op)
    (if (not (eq? input password)) warning
        (begin
          (set! wrong-times 0)
          (cond ((eq? op 'withdraw) withdraw)
                ((eq? op 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" op))))
        ))
  dispatch)

;TEST
(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'adfdsf 'diposit) 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rand (random 1000000))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
  
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter n ans)
    (if (= n 0) ans
        (if (experiment)
            (iter (- n 1) (+ ans 1))
            (iter (- n 1) ans))))
  (/ (iter trials 0.0) trials))

(estimate-pi 100000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5
(define (random-in-range low high);;[low, high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 1 10)


(define A (make-account 10 'a))
(define B A)
((B 'a 'withdraw) 10)
((A 'a 'deposit) 100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;7
(define (make-joint  origin-acc origin-password another-password)
  (lambda (given-password mode)
    (if (eq? given-password another-password)
        (origin-acc origin-password mode)
        display-wrong-another-password-message)))

(define (display-wrong-another-password-message . useless-arg)
    "Incorrect another password")

;;TEST
(define a (make-account 1000 'secret))
((a 'secret 'withdraw) 10)
(define b (make-joint a 'secret 'newpwd))
((b 'secret 'withdraw) 10 10)
((b 'newpwd 'withdraw) 10)
((a 'secret 'deposit) 300)
((b 'newpwd 'withdraw) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;8
(define f
  (let ((ans 1))
    (lambda (x)
      (set! ans (* ans x))
      ans)))

(+ (f 0) (f 1))
;;from right to left . ans is 1
