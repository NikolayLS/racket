(define (positionCheck binaryNumber digit)
  (remainder (quotient binaryNumber (expt 10 digit)) 2))

(define (intersect y)
     (if (= y 0) 0
         (+ (if (= (remainder y 10) 2) 1 0) (* 10 (intersect (quotient y 10))))))

(define (difference x y)
  (if (= x 0)
      0
      (+ (if (= (remainder x 10) (remainder y 10))
             0
             (remainder x 10))
         (* 10 (difference (quotient x 10) (quotient y 10))))))

(define (union y)
     (if (= y 0) 0
         (+ (if (or (= (remainder y 10) 2) (= (remainder y 10) 1)) 1 0) (* 10 (union (quotient y 10))))))

(define (toBinary x)
  (if (= x 0) 0
      (+ (remainder x 2) (* 10 (toBinary (quotient x 2))))))

(define (numberOfDigits y)
  (if (= y 0) 0
      (+ 1 (numberOfDigits (quotient y 10)))))

(define (numberOfOnes x)
  (if (= x 0)
      0
      (+ (remainder x 2) (numberOfOnes (quotient x 10)))))

(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

(define (set-add set element)
 (if (> 0 element)
      #f
     (if (> element (numberOfDigits (toBinary set)))
         (toDecimal (+ (expt 10 element) (toBinary set)))
         (if (= (positionCheck (toBinary set) element) 0)
             (toDecimal (+ (expt 10 element) (toBinary set)))
             set))))

(define (set-remove set element)
   (if (or (> element (numberOfDigits (toBinary set))) (> 0 element) (= (positionCheck (toBinary set) element) 0))
       #f
       (toDecimal (- (toBinary set) (expt 10  element)))))

(define (set-contains set element)
  (if (> 0 element)
      #f
      (if  (= (positionCheck (toBinary set) element) 1)
           #t
           #f)))

(define (set-empty set)
  (if (= (numberOfDigits (toBinary set)) 0)
              #t
              #f))

(define (set-size set)
  (numberOfOnes (toBinary set)))

(define (set-intersect s1 s2)
  (define psevdoSum (+ (toBinary s1) (toBinary s2)))
   (toDecimal (intersect psevdoSum)))

(define (set-union s1 s2)
   (define psevdoSum (+ (toBinary s1) (toBinary s2)))
     (toDecimal (union psevdoSum)))

(define (set-difference s1 s2)
        (toDecimal (difference (toBinary s1) (toBinary s2))))
        
(define (knapsack c n w p)
  (if (or (= n 0) (=c 0))
      0
      (if (< (w (- n 1) c))
          (knapsack c (- n 1) w p)
          (max (+ (p (- n 1)) (knapsack (- c (w (- n 1))) (- n 1) w p)) (knapsack c (- n 1) w p)))))