#lang racket

(define SMALL 1)
(define BIG 100)

(define (get-middle)
  (arithmetic-shift (+ SMALL BIG) -1))

(define (guess-my-number)
  (displayln (get-middle)))

(define (smaller)
  (set! BIG (sub1 (get-middle)))
  (guess-my-number))

(define (bigger)
  (set! SMALL (add1 (get-middle)))
  (guess-my-number))

(define (start-over)
  (set! SMALL 1)
  (set! BIG 100)
  (guess-my-number))