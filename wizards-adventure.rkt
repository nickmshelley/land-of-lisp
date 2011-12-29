#lang racket

(define nodes '((living-room (You are in the living room. A wizard is snoring loudly on the couch.))
                (garden (You are in a beautiful garden. There is a well in front of you.))
                (attic (You are in the attic. There is a giant welding torch in the corner.))))

(define edges '((living-room (garden west door)
                             (attic upstairs ladder))
                (garden (living-room east door))
                (attic (living-room downstairs ladder))))

(define objects '(whiskey bucket frog chain))

(define location 'living-room)

(define (walk direction)
  (define (has-direction? l)
    (eq? direction (second l)))
  (define next
    (findf has-direction? (rest (assoc location edges))))
  (if next
      (begin (set! location (first next))
             (look location))
      '(You can't go that direction)))

(define object-locations '((whiskey living-room)
                           (bucket living-room)
                           (chain garden)
                           (frog garden)))

(define (objects-at loc objs obj-locs)
  (define (at-location? obj)
    (eq? loc (second (assoc obj obj-locs))))
  (filter at-location? objs))

(define (describe-objects loc objs objs-loc)
  (define (describe-object obj)
    `(You see a ,obj on the floor.))
  (append-map describe-object (objects-at loc objs objs-loc)))

(define (pickup object)
  (cond 
    [(member object (objects-at location objects object-locations))
     (set! object-locations (cons (list object 'body) object-locations))
     `(You are now carrying the ,object)]
    [else '(You cannot get that)]))

(define (inventory)
  (cons 'items: (objects-at 'body objects object-locations)))

(define (describe-location location nodes)
  (first (rest (assoc location nodes))))

(define (describe-path edge)
  `(There is a ,(third edge) going ,(second edge) from here.))

(define (describe-paths location edges)
  (append-map describe-path (rest (assoc location edges))))

(define (look loc)
  (append (describe-location loc nodes)
          (describe-paths loc edges)
          (describe-objects loc objects object-locations)))