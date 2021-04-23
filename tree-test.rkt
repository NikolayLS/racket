#lang racket/base
(require rackunit rackunit/gui)
(require "tree.rkt")
;Николай Стефанов 45545


(define (correctTree string)
  (test-true (string-append string " is Correct")
             (tree? string)))

(define (incorrectTree string)
  (test-false (string-append string " is Incorrect")
              (tree? string)))

(define (correctBalanced tree)
  (test-true (string-append (tree->string tree) " is Balanced")
             (balanced? tree)))

(define (incorrectBalanced tree)
  (test-false (string-append (tree->string tree) " is not Balanced")
              (balanced? tree)))

(define (correctOrdered tree)
  (test-true (string-append (tree->string tree) "is Ordered")
             (ordered? tree)))

(define (incorrectOrdered tree)
  (test-false (string-append (tree->string tree) " is not Ordered")
              (ordered? tree)))

(define (testTree->string tree string)
  (test-true (string-append (tree->string tree)" is Correct!")
             (equal? (tree->string tree) string)))

(test/gui
 (test-suite "Tree? test"
  (correctTree "*")
  (correctTree "{       333*       *}")
  (correctTree "{33     {33333  *  *}*      }")
  (correctTree "{3      {3 **}{3**}  }")
  (correctTree "{3      {3 {3**}        {3*      {3 *{3*    *}}}}{3*{3*    *}}   }")
  (incorrectTree "{*}")
  (incorrectTree "{3  3*}")
  (incorrectTree "{33}")
  (incorrectTree "{3*}*")
  (incorrectTree "3333"))
  
 (test-suite "Balance? test"
  (correctBalanced '(33 (3 (33 () ()) ()) (33 () ())))
  (correctBalanced '(33 (3 () ()) ()) )
  (correctBalanced '(3333  (33  (33 () ())  (33 () ()))  (33  (33 () ())  (33 () ()))))
  (correctBalanced '(3 (33 () ()) (333 () ())) )
  (correctBalanced '(3 (33 () ()) (333 (222()()) (444()()) )) )
  (incorrectBalanced '(23 (33 (43 () ()) ())()) )
  (incorrectBalanced '(23 (33 (43 () ()) () ) (3 (4 (5 () () ) () ) () ) ) )
  (incorrectBalanced '(2 () (3 (4 () ())(33 () (33 () (33 () ()))))))
  (incorrectBalanced '(2 (33 (33(33 () ()) ()) ()) (3 (4 () ())()))) 
  (incorrectBalanced '(2 (33 (33(33 () ()) ()) (33 (33 (33 ()(33 () ())) ()) ())) (3 (4 () ())()))))
 
 (test-suite "Ordered? test"
  (correctOrdered '() )
  (correctOrdered '(33 () ()) )
  (correctOrdered '(33 (22 (11 () ()) (23 () ())) (2222 (1111 () ()) ())))
  (correctOrdered '(33 (22 (11 (10 () ()) ()) (23 () ())) (2222 (1111 () ()) ())))
  (correctOrdered '(33 (22 (11 () (12 (12 () ()) ())) (23 () ())) (2222 (1111 () ()) ())))
  (incorrectOrdered '(33 (12111 (1234 () ()) (3333 () ())) (1 (456 () ()) ())))
  (incorrectOrdered '(1234 (12345 () ()) ()) )
  (incorrectOrdered '(123 (1234 () ()) (1237 () (1234 () ()))) )
  (incorrectOrdered '(123 (1234 () ()) (1237 (1 () ()) (1234 () ()))) )
  (incorrectOrdered '(1234 () (1 () ()))))

 (test-suite "Tree->string test"
  (testTree->string '() "*")
  (testTree->string '(3 () ()) "{3 * *}")
  (testTree->string '(3 (3 (3 () ()) ()) ()) "{3 {3 {3 * *} *} *}")
  (testTree->string '(3 (3 (3 () ()) ()) (3 (3 () ()) ())) "{3 {3 {3 * *} *} {3 {3 * *} *}}")
  (testTree->string '(3 (3 () ()) (3 ()())) "{3 {3 * *} {3 * *}}")))


         

