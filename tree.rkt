#lang racket/base
(require racket/stream)
(provide (all-defined-out))

;Николай Стефанов 45545


;помощни функции
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (numberToString number)
  (define (numberToString* number)
    (if (= number 0)
        ""
        (string-append
         (numberToString* (quotient number 10))
         (fromNumberToString (remainder number 10)))))
  (if (= number 0)
      "0"
      (numberToString* number)))
      
(define(fromCharToNumber c)
 (cond ((char=? c #\0) 0)
       ((char=? c #\1) 1)
       ((char=? c #\2) 2)
       ((char=? c #\3) 3)
       ((char=? c #\4) 4)
       ((char=? c #\5) 5)
       ((char=? c #\6) 6)
       ((char=? c #\7) 7)
       ((char=? c #\8) 8)
       ((char=? c #\9) 9)))

(define (fromNumberToString c)
  (cond
    ((= c 1) "1")
    ((= c 2) "2")
    ((= c 3) "3")
    ((= c 4) "4")
    ((= c 5) "5")
    ((= c 6) "6")
    ((= c 7) "7")
    ((= c 8) "8")
    ((= c 9) "9")
    ((= c 0) "0")))

(define (stringToNumber string)
  (define (stringToNumber* string index)
    (if(= index (string-length string))
       0
       (+
        (* (expt 10 (- (string-length string) (+ index 1))) (fromCharToNumber (string-ref string index)))
        (stringToNumber* string (+ 1 index)))))
  (stringToNumber* string 0))

(define (char-digit? c)
  (and
   (char>=? c #\0)
   (char<=? c #\9)))

(define (char-space? c)
  (if (char=? c #\space)
      #t
      #f))

(define (char-open-bracket? c)
  (if (char=? c #\{)
      #t
      #f))

(define (char-close-bracket? c)
  (if (char=? c #\})
      #t
      #f))

(define (char-star? c)
  (if (char=? c #\*)
      #t
      #f))

(define (1+ x) (+ x 1))

(define (space-remover str index)
  (cond [(= index (string-length str)) str]
        [(char-space? (string-ref str index))
         (space-remover (string-append
                         (substring str 0 index)
                         (substring str (+ 1 index) (string-length str)))
                        index)]
        [else (space-remover str (+ 1 index))]))
        
(define (remove-spaces str)
  (space-remover str 0))

;функция която проверява дали "просто дърво/ листо" е валидно
;след това ще я използваме във функция която търси "прости дървета / листа"
(define (simpleTreeValid? string index case)
  (cond
    [(= case 0) (cond [(char-open-bracket? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ case))]
                         [else #f])]
    [(= case 1) (cond [(char-space? (string-ref string index)) (simpleTreeValid? string (1+ index) case)]
                         [(char-digit? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ case))]
                         [else #f])]
    [(= case 2) (cond [(char-digit? (string-ref string index)) (simpleTreeValid? string (1+ index) case)]
                         [(char-space? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ case))]
                         [(char-star? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ (1+ case)))]
                         [else #f])]
    [(= case 3) (cond [(char-space? (string-ref string index)) (simpleTreeValid? string (1+ index) case)]
                         [(char-star? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ case))]
                         [else #f])]
    [(= case 4) (cond [(char-space? (string-ref string index)) (simpleTreeValid? string (1+ index) case)]
                         [(char-star? (string-ref string index)) (simpleTreeValid? string (1+ index) (1+ case))]
                         [else #f])]
    [(= case 5) (cond [(char-space? (string-ref string index)) (simpleTreeValid? string (1+ index) case)]
                         [(char-close-bracket? (string-ref string index)) index]
                         [else #f])]
    ))


;функция която търси листа и ако листото е валидно го замества със "*"
;ако дървото е валидно, то накрая би останал само символът "*"
(define (isThereValid str index index1)
  (cond ((= index (string-length str)) str)
        ((char-open-bracket? (string-ref str index)) (isThereValid str (1+ index) index))
        ((char-close-bracket? (string-ref str index)) (if
                                                       (simpleTreeValid? (substring str index1 (1+ index)) 0 0)
                                                       (isThereValid 
                                                        (string-append (substring str 0 index1) "*" (substring str (1+ index) (string-length str)))
                                                        0
                                                        0)
                                                       #f))                                 
        (else (isThereValid str  (1+ index) index1))))

(define (tree? str)
  (if (equal? (isThereValid str 0 0) #f)
      #f
      (if (equal? (remove-spaces (isThereValid str 0 0)) "*")
          #t
          #f)))

;функция която намира другарчето на скоба
(define (whereIsMyFriend string index br)
  (cond [(= br 0) (- index 1)]
        [(char-open-bracket? (string-ref string index))
         (whereIsMyFriend string (1+ index) (1+ br))]
        [(char-close-bracket? (string-ref string index))
         (whereIsMyFriend string (1+ index) (- br 1))]
        [else (whereIsMyFriend string (1+ index) br)]))

;функция която по "голямо дърво" връща корена му (като стринг)
(define (making-root string)
  (define (making-root* string index)
    (if (char-digit? (string-ref string index))
        (making-root* string (1+ index))
        (substring string 1 index)))
  (making-root* string 1))

;функция която по "голямо дърво" връща лявото му поддърво (като стринг)
(define (making-leftSub string)
  (define (making-leftSub* string index)
    (cond [(char-star? (string-ref string index))
           "*"]
          [(char-open-bracket? (string-ref string index))
           (substring string
                      index
                      (1+ (whereIsMyFriend string (1+ index) 1)))]
          [else (making-leftSub* string (1+ index))]))
  (making-leftSub* string (1+ (string-length (making-root string)))))

;функция която по "голямо дърво" връща дясното му поддърво (като стринг)
(define (making-rightSub string)
  (define (making-rightSub* string index)
    (cond [(char-star? (string-ref string index))
           "*"]
          [(char-open-bracket? (string-ref string index))
           (substring string
                      index
                      (1+ (whereIsMyFriend string (1+ index) 1)))]
          [else (making-rightSub* string (1+ index))]))
  (making-rightSub* string (+ (string-length (making-root string))
                              (1+ (string-length (making-leftSub string))))))
;функция (която използва горните 3 помощни функции)и прави от стринг -> дърво
(define (buildTree string lst)
  (append lst
          (list (stringToNumber (making-root string)))         
          (if (equal? (making-leftSub string) "*")
              (append lst (list '()))
              (append lst (list (buildTree (making-leftSub string) lst))))
          (if (equal? (making-rightSub string) "*")
              (append lst (list '()))
              (append lst (list (buildTree (making-rightSub string) lst))))))

(define (string->tree str)
  (if (tree? str)
      (buildTree (remove-spaces str) '())
      #f))
  
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

(define (balanced? t)
  (cond
    [(empty-tree? t) #t]
    [else(and (<= (abs(- (height (right-tree t)) (height (left-tree t)))) 1)
              (balanced? (right-tree t))
              (balanced? (left-tree t)))]))

;проверява дали всички върхове на дърво са не по-големи от дадено цяло число 
(define (higherInteger tree integer)
  (cond
    [(empty-tree? tree) #t]
    [else (and (>= integer (root-tree tree))
               (higherInteger (left-tree tree) integer)
               (higherInteger (right-tree tree) integer))]))

;проверява дали всички върхове на дърво са по-малки от дадено цяло число
(define (lowerInteger tree integer)
  (cond
    [(empty-tree? tree) #t]
    [else (and (< integer (root-tree tree))
               (lowerInteger (left-tree tree) integer)
               (lowerInteger (right-tree tree) integer))]))

;използвме горните две функции
;за всеки връх проверяваме дали всички от ляво са не по-големи от него и всички от дясно са по-големи
(define (ordered? tree)
  (cond
    [(empty-tree? tree) #t]
    [else (and (higherInteger (left-tree tree) (root-tree tree))
               (lowerInteger (right-tree tree) (root-tree tree))
               (ordered? (left-tree tree))
               (ordered? (right-tree tree)))]))


(define (tree->string tree)
  (if (empty-tree? tree)
      "*"
      (string-append
       "{"
       (numberToString (root-tree tree))
       " "
       (tree->string (left-tree tree))
       " "
       (tree->string (right-tree tree))
       "}")))


(define (preorder tree)
  (cond [(empty-tree? tree) '()]
        [else (append (list (root-tree tree))
                      (preorder (left-tree tree))
                      (preorder (right-tree tree)))]))

(define (inorder tree)
  (cond [(empty-tree? tree)'()]
        [else (append (inorder (left-tree tree))
                      (list (root-tree tree))
                      (inorder (right-tree tree)))]))

(define (postorder tree)
  (cond [(empty-tree? tree) '()]
        [else (append (postorder (left-tree tree))
                      (postorder (right-tree tree))
                      (list (root-tree tree)))]))

(define (tree->stream tree order)
  (cond [(equal? order 'preorder) (stream-first(stream(preorder tree)))]
        [(equal? order 'inorder)  (stream-first(stream(inorder tree)))]
        [(equal? order 'postorder)(stream-first(stream(postorder tree)))]
        [else #f]))





