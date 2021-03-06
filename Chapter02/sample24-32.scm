;; 可以测试x中共有多少个叶子结点
(define (print x) (newline) (display x))
(define (count-leaves x)
  (print x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(list x x)
(count-leaves (list x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;24
(count-leaves (list 1 (list 2 (list 3 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;27
(define (tree-reverse lst)
  (define (iter remained-items result)
    (if (null? remained-items)
        result
        (let ((value (car remained-items)))
          (iter (cdr remained-items)
                (cons (if (pair? value)
                          (tree-reverse value)
                          value)
                      result)))))
  (iter lst '()))

(tree-reverse (list (list 1 2) (list 3 4) (list 3 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;28
;;;return a list that contain all leaves of a tree

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

(define x (list (list 3 4) (list 5 6 7)))
(fringe (list x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;29

(define (make-mobile left right) (list left right))
(define (make-branch length struct) (list length struct))
(define (left-branch x) (car x))
(define (right-branch x) (cadr x))
(define (branch-length x) (car x))
(define (branch-struct x) (cadr x))
(define (total-weight x)
  (cond ((null? x) 0)
        ((not (pair? (car x))) (branch-struct x))
        (else (+ (total-weight (left-branch x))
                 (total-weight (right-branch x))))))

(define mobile (make-mobile (make-branch 10 20)
                            (make-branch 10 25)))

(total-weight mobile)

(define (branch-torque x)
  (* (branch-length x)
     (branch-weight x)))

(define (is-balance? x)
  (if (not (pair? (car x)))
      (cons (branch-struct x) true)
      (let ((l (left-branch x))
            (r (right-branch x)))
        (let ((lans (is-balance? l))
              (rans (is-balance? r)))
          (if (and (cdr lans)
                   (cdr rans)
                   (= (* (car lans) (branch-length l))
                      (* (car rans) (branch-length r))))
              (cons (+ (car lans) (car rans)) true)
              (cons 0 false))))))

(define mobile (make-mobile (make-branch 15 20)
                            (make-branch 10 30)))
; return (total-weight #t) if it is, (0 #f) if not
(is-balance? mobile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;30
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;31
(define (tree-map func tree)
  (map (lambda (tree)
         (if (pair? tree)
             (tree-map func tree)
             (func tree)))
       tree))

(tree-map square tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;32
(define (subsets s)
  (if (null? s) (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))

(subsets (list (list 1 2) 3 4))                               
                               
                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
      
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate opt init x)
  (if (null? x) init
      (opt (car x)
           (accumulate opt init (cdr x)))))

(define (filter pred x)
  (cond ((null? x) '())
        ((pred (car x))
         (cons (car x)
               (filter pred (cdr x))))
        (else (filter pred (cdr x)))))

(define test (list 1 (list 2 (list 3 4) 5)))      

(define (sum-odd-square tree)
  (accumulate + 0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-square test)

(define (even-func func n)
  (accumulate cons '()
              (filter even?
                      (map func
                           (enumerate-interval 0 n)))))           
(even-func square 10)
(filter even?
        (map square
             (enumerate-interval 0 10)))

