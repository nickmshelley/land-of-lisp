#lang racket
(require "../graph-utils/graph-utils.rkt"
         tests/eli-tester)

(define nodes empty)
(define edges empty)
(define visited-nodes empty)
(define node-num 30)
(define edge-num 45)
(define worm-num 3)
(define cop-prob 15)

(define (make-edge node1 node2)
  (if (eq? node1 node2)
      empty
      (list (cons node1 node2) (cons node2 node1))))
(test
 (make-edge 'a 'b) => '((a . b) (b . a))
 (make-edge 'a 'a) => empty)

(define (random-node)
  (add1 (random node-num)))

(define (make-edge-list)
  (apply append
         (for/list ([_ (in-range edge-num)])
           (make-edge (random-node) (random-node)))))