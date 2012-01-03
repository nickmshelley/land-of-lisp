#lang racket
(require tests/eli-tester)

(define nodes empty)
(define edges empty)
(define visited-nodes empty)
(define node-num 30)
(define edge-num 45)
(define worm-num 3)
(define cop-prob 15)

(define (make-edge node1 node2)
  (list (cons node1 node2) (cons node2 node1)))