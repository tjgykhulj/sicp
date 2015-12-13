(load "~/scheme/2/sample24-32.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;33
(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))
(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2 3 4) (list 5 6 7 8))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(length (list 1 2 (list 3 4) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;34
(define (horner-eval x c-seq)
  (accumulate (lambda (this-c higher-terms)
                (* x
                   (+ this-c higher-terms)))
              0 c-seq))
(horner-eval 2 (list 1 3 0 5 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (tree)
                     (if (pair? tree)
                         (count-leaves tree)
                         1))
                   t)))
(count-leaves (list (list 2 4) (list 1 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (car-n seqs))
              (accumulate-n op init (cdr-n seqs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prime? n)
  (define sq-n (sqrt n))
  (define (iter i)
    (if (> i sq-n)
        true
        (if (= 0 (remainder n i))
            false
            (iter (+ i 1)))))
  (iter 2)
  )
        
  
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;40
;; the key is to make-pair(i,j) (1<=j<i<=n)
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;41
(define (uniq-3 n s)
  
  (define (judge? pair)
    (let ((a1 (car pair))
          (a2 (cadr pair)))
      (let ((a3 (- s a1 a2)))
        (not (or (< a3 1)
                 (= a1 a2)
                 (= a1 a3)
                 (= a2 a3))))))

  (map (lambda (pair)
         (list (car pair) (cadr pair) (- s (car pair) (cadr pair))))
       (filter judge?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 n)))
                (enumerate-interval 1 n)))))

(uniq-3 10 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;42
(define (queens board-size)

  (define (safe? k position)
    (iter-check (car position) 
                (cdr position)
                 1))

  (define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
          (if (or (= row-of-new-queen row-of-current-queen)
                  (= row-of-new-queen (+ row-of-current-queen i)) 
                  (= row-of-new-queen (- row-of-current-queen i)))
              #f
              (iter-check row-of-new-queen 
                          (cdr rest-of-queens)
                          (+ i 1))))))
  
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

  (define empty-board '())
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;43
;; 一轮增大了board-size倍，一共有board-size轮。
;; 故为T * (board-size ** board-size)

