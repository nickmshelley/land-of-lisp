#lang racket

(define nodes '((living-room (You are in the living room. A wizard is snoring loudly on the couch.))
                (garden (You are in a beautiful garden. There is a well in front of you.))
                (attic (You are in the attic. There is a giant welding torch in the corner.))))

(define edges '((living-room (garden west door)
                             (attic upstairs ladder))
                (garden (living-room east door))
                (attic (living-room downstairs ladder))))

(define (describe-location location nodes)
  (first (rest (assoc location nodes))))

(define (describe-path edge)
  `(There is a ,(third edge) going ,(second edge) from here.))

(define (describe-paths location edges)
  (append-map describe-path (rest (assoc location edges))))