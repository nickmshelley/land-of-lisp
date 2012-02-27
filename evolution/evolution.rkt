#lang racket
(require 2htdp/universe
         2htdp/image)

(define size 100)
(define land (empty-scene size size))
(define jungle '(45 45 10 10))
(define plant-energy 80)
(define reproduction-energy 200)
(define animal-pic (rectangle 5 5 "solid" "brown"))
(define plant-pic (rectangle 4 4 "solid" "green"))

(struct world (plants animals) #:transparent)
(struct animal (pos energy direction genes) #:transparent)
(struct posn (x y) #:transparent)

(define (render st)
  (define plant-scene
    (for/fold ([scene land])
      ([pl (in-hash-keys (world-plants st))])
      (place-image plant-pic
                   (posn-x pl)
                   (posn-y pl)
                   scene)))
  (for/fold ([scene plant-scene])
    ([anim (world-animals st)])
    (define p (animal-pos anim))
    (place-image animal-pic
                 (posn-x p)
                 (posn-y p)
                 scene)))
             
(define (tick-handler st)
  (define h (world-plants st))
  (define new-animals
    (map (lambda (a)
           (handle-animal a h))
         (filter not-dead? (world-animals st))))
  (define reproduced-animals (reproduce new-animals))
  (new-plants! h)
  (world h reproduced-animals))
  
(define (not-dead? anim)
  (> (animal-energy anim) 0))

(define (reproduce animals)
  (define origs
    (for/list ([anim animals]
               #:when (> (animal-energy anim) reproduction-energy))
      anim))
  (define new
    (for/list ([anim origs])
      (list (struct-copy animal anim [energy (arithmetic-shift (animal-energy anim) -1)])
            (struct-copy animal anim [energy (arithmetic-shift (animal-energy anim) -1)]
                         [genes (mutate (animal-genes anim))]))))
  (append* (remove* origs animals) new))

(define (mutate gns)
  (define mut (random (length gns)))
  (for/list ([i (in-range (length gns))]
             [item gns])
    (if (= i mut)
        (max 1 (+ item (random 3) -1))
        item)))
     
(define (handle-animal anim plants)
  (eat (move (turn anim)) plants))

(define (turn anim)
  (define gns (animal-genes anim))
  (define rand (random (apply + gns)))
  (define ang
    (let angle ([x rand]
                [gs gns])
      (define new-x (- x (first gs)))
      (if (< new-x 0)
          0
          (add1 (angle new-x (rest gs))))))
  (struct-copy animal anim 
               [direction (modulo (+ (animal-direction anim) ang) 
                                    (length gns))]))

(define (move anim)
  (define p (animal-pos anim))
  (define dir (animal-direction anim))
  (define new-x
    (cond
      [(and (>= dir 2) (<= dir 4))
       (add1 (posn-x p))]
      [(and (>= dir 6) (<= dir 8))
       (sub1 (posn-x p))]
      [else (posn-x p)]))
  (define new-y
    (cond
      [(and (>= dir 0) (<= dir 2))
       (add1 (posn-y p))]
      [(and (>= dir 4) (<= dir 6))
       (sub1 (posn-y p))]
      [else (posn-y p)]))
  (struct-copy
   animal anim
   [pos (posn (modulo new-x size)
              (modulo new-y size))]))

(define (eat anim plants)
  (define p (animal-pos anim))
  (cond
    [(hash-has-key? plants p)
     (hash-remove! plants p)
     (struct-copy
      animal anim
      [energy (+ (animal-energy anim) plant-energy)])]
    [else 
     (struct-copy
      animal anim
      [energy (sub1 (animal-energy anim))])]))

(define (new-plants! h)
  (for ([_ (in-range 2)])
    (apply random-plant! h jungle))
  (for ([_ (in-range 1)])
    (random-plant! h 0 0 size size)))

(define (random-plant! h left top width height)
  (hash-set! h (posn (+ left (random width))
                     (+ top (random height)))
             #t))

(define (key-handler st k)
  (cond
    [(key=? k "p")
     (print-stuff st)
     st]
    [(key=? k "a")
     (do-tick st 1000)]
    [(key=? k "b")
     (do-tick st 10000)]
    [(key=? k "d")
     (do-tick st 100000)]
    [(key=? k "e")
     (do-tick st 1000000)]
    [else st]))

(define (print-stuff st)
  (printf "animals:~n")
  (for ([anim (world-animals st)])
    (printf "~a~n" (animal-genes anim))))

(define (do-tick st num)
  (do-tick-helper st num num))

(define (do-tick-helper st num orig)
  (when (= (modulo num 1000) 0)
      (printf "~a% complete...~n" (real->decimal-string (/ (- orig num) orig 1/100))))
  (if (= num 0)
      st
      (do-tick-helper (tick-handler st) (sub1 num) orig)))

(big-bang (world (make-hash)
                 (list (animal (posn (arithmetic-shift size -1)
                                     (arithmetic-shift size -1))
                               500 1 '(1 1 1 1 1 1 1 1))))
          (to-draw render)
          (on-tick tick-handler)
          (on-key key-handler))