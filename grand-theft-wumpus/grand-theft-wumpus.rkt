#lang racket
(require "../graph-utils/graph-utils.rkt"
         tests/eli-tester)

(define nodes empty)
(define edges empty)
(define visited-nodes empty)
(define player-pos 0)
(define node-num 30)
(define edge-num 45)
(define worm-num 3)
(define cop-prob 15)

(define (edge-pair node1 node2)
  (if (eq? node1 node2)
      empty
      (list (cons node1 node2) (cons node2 node1))))
(test
 (edge-pair 'a 'b) => '((a . b) (b . a))
 (edge-pair 'a 'a) => empty)

(define (random-node)
  (add1 (random node-num)))

(define (make-edge-list)
  (remove-duplicates
   (apply append
          (for/list ([_ (in-range edge-num)])
            (edge-pair (random-node) (random-node))))))

(define (nodes-from node edges)
  (map cdr (filter (lambda (edge)
                     (eq? (car edge) node))
                   edges)))
(test
 (nodes-from 1 '((1 . 2) (2 . 3) (1 . 4) (3 . 1)))
 => '(2 4))

(define (get-connected node edges)
  (let traverse ([remaining (list node)]
                 [visited empty])
    (cond 
      [(empty? remaining) (reverse visited)]
      [(member (first remaining) visited)
       (traverse (rest remaining) visited)]
      [else (traverse (append (rest remaining) (nodes-from (first remaining) edges))
                      (cons (first remaining) visited))])))
(test
 (get-connected 1 '((1 . 2) (2 . 3))) => '(1 2 3)
 (get-connected 1 '((1 . 3) (3 . 1) (1 . 4) (4 . 1) (3 . 5) (5 . 3) (5 . 6) (6 . 5) (4 . 7) (7 . 4) (5 . 7) (7 . 5)))
 => '(1 3 4 5 7 6))

(define (find-islands nodes edges)
  (let loop ([islands empty]
             [unconnected nodes])
    (cond 
      [(empty? unconnected) (reverse islands)]
      [else (define connected (get-connected (first unconnected) edges))
            (loop (cons connected islands) (remove* connected unconnected))])))
(test
 (find-islands '(1 2) '()) => '((1) (2))
 (find-islands '(1 2) '((1 . 2) (2 . 1))) => '((1 2))
 (find-islands '(1 2 3 4) 
               '((1 . 2) (2 . 1) (3 . 4) (4 . 3)))
 => '((1 2) (3 4))
 (find-islands '(1 2 3 4) 
               '((1 . 2) (2 . 1) (2 . 3) (3 . 2) (3 . 4) (4 . 3)))
 => '((1 2 3 4)))

(define (connect-with-bridges islands)
  (if (empty? (rest islands))
      empty
      (append (edge-pair (last (first islands)) (first (first (rest islands))))
              (connect-with-bridges (rest islands)))))
(test 
 (connect-with-bridges '((1 2 3 4))) => '()
 (connect-with-bridges '((1 2) (3 4) (5 6)))
 => '((2 . 3) (3 . 2) (4 . 5) (5 . 4)))

(define (connect-all-islands nodes edges)
  (append edges (connect-with-bridges (find-islands nodes edges))))
(test
 (connect-all-islands '(1 2) '()) => '((1 . 2) (2 . 1))
 (connect-all-islands '(1 2 3 4 5 6)
                      '((1 . 2) (2 . 1) (3 . 4) (4 . 3) (5 . 6) (6 . 5)))
 => '((1 . 2) (2 . 1) (3 . 4) (4 . 3) (5 . 6) (6 . 5) (2 . 3) (3 . 2) (4 . 5) (5 . 4)))

(define (make-city-edges)
  (define nodes (sequence->list (in-range 1 (add1 node-num))))
  (define edges (connect-all-islands nodes (make-edge-list)))
  (define cops (filter (lambda (edge)
                         (and (< (car edge) (cdr edge))
                              (zero? (random cop-prob))))
                       edges))
  (add-cops (edges-to-alist nodes edges) cops))

(define (edges-to-alist nodes edges)
  (map (lambda (node)
         (cons node (map list (nodes-from node edges))))
       nodes))
(test
 (edges-to-alist '(1 2 3) '((1 . 2) (2 . 1) (2 . 3) (3 . 2)))
 => '((1 (2)) (2 (1) (3)) (3 (2))))

(define (add-cops edge-alist cop-edges)
  (map (lambda (x)
         (define node1 (first x))
         (define node1-edges (rest x))
         (cons node1 (map (lambda (edge)
                            (define node2 (first edge))
                            (define pair (edge-pair node1 node2))
                            (if (or (member (first pair) cop-edges)
                                    (member (second pair) cop-edges))
                                (list node2 'cops)
                                edge))
                          node1-edges)))
       edge-alist))
(test
 (add-cops '((1 (2)) (2 (1) (3)) (3 (2))) '((2 . 3)))
 => '((1 (2)) (2 (1) (3 cops)) (3 (2 cops)))
 (add-cops '((1 (2)) (2 (1) (3)) (3 (2))) '((3 . 2)))
 => '((1 (2)) (2 (1) (3 cops)) (3 (2 cops)))
 (add-cops '((1 (2)) (2 (1) (3)) (3 (2))) '((2 . 1) (2 . 3) (1 . 2)))
 => '((1 (2 cops)) (2 (1 cops) (3 cops)) (3 (2 cops))))

(define (neighbors node edge-alist)
  (map car (rest (assoc node edge-alist))))
(test
 (neighbors 2 '((1 (2)) (2 (1) (3)) (3 (2)))) => '(1 3))

(define (within-one a b edge-alist)
  (member b (neighbors a edge-alist)))
(test
 (within-one 1 3 '((1 (2)) (2 (1) (3)) (3 (2)))) => #f
 (within-one 1 2 '((1 (2)) (2 (1) (3)) (3 (2)))) => '(2))

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist)
      (memf (lambda (node)
              (within-one node b edge-alist))
            (neighbors a edge-alist))))
(test
 (within-two 1 3 '((1 (2)) (2 (1) (3)) (3 (2)))) => '(2)
 (within-two 1 4 '((1 (2)) (2 (1) (3)) (3 (2) (4)) (4 (3)))) => #f)

(define (make-city-nodes edge-alist)
  (define wumpus (random-node))
  (define glow-worms
    (for/list ([i (in-range worm-num)])
      (random-node)))
  (for/list ([n (in-range 1 (add1 node-num))])
    (append (list n)
            (cond
              [(= n wumpus) '(wumpus)]
              [(within-two n wumpus edge-alist) '(blood)]
              [else empty])
            (cond
              [(member n glow-worms) '(glow-worm)]
              [(memf (lambda (worm)
                       (within-one n worm edge-alist))
                     glow-worms)
               '(lights)]
              [else empty])
            (if (memf (lambda (edge)
                        (not (empty? (rest edge))))
                      (rest (assoc n edge-alist)))
                '(sirens)
                empty))))

(define (find-empty-node node-alist)
  (define x (random-node))
  (if (empty? (rest (assoc x node-alist)))
      x
      (find-empty-node node-alist)))

(define (new-game)
  (set! edges (make-city-edges))
  (set! nodes (make-city-nodes edges))
  (set! player-pos (find-empty-node nodes))
  (set! visited-nodes (list player-pos))
  (draw-known-city))

(define (draw-city)
  (ugraph->png "city" nodes edges))

(define (draw-known-city)
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(define (known-city-nodes)
  (map (lambda (node)
         (cond
           [(eq? node player-pos)
            (append (assoc node nodes) '(*))]
           [(member node visited-nodes)
            (assoc node nodes)]
           [else (list node '?)]))
       (remove-duplicates 
        (append visited-nodes 
                (append-map (lambda (node)
                              (neighbors node edges))
                            visited-nodes)))))

(define (known-city-edges)
  (map (lambda (node)
         (cons node (map (lambda (edge)
                           (if (member (first edge) visited-nodes)
                               edge
                               (list (first edge))))
                         (rest (assoc node edges)))))
       visited-nodes))

(define (walk pos)
  (handle-direction pos #f))

(define (charge pos)
  (handle-direction pos #t))

(define (handle-direction pos charging)
  (define edge (assoc pos (rest (assoc player-pos edges))))
  (if edge
      (handle-new-place edge pos charging)
      (display "That place does not exist")))

(define (handle-new-place edge pos charging)
  (define node (assoc pos nodes))
  (define has-worm (and (member 'glow-worm node)
                        (not (member pos visited-nodes))))
  (set! player-pos pos)
  (set! visited-nodes (remove-duplicates (cons pos visited-nodes)))
  (draw-known-city)
  (cond
    [(member 'cops edge)
     (display "You ran into the cops. Game over.")]
    [(member 'wumpus node)
     (if charging
         (display "You found the Wumpus! You win!")
         (display "You ran into the Wumpus"))]
    [charging
     (display "You wasted your last bullet. Game over.")]
    [has-worm
     (define new-pos (random-node))
     (printf "You ran into the glow-worm gang! You're now at ~a" new-pos)
     (handle-new-place empty new-pos #f)]))