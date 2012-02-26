#lang racket

(struct state (player monsters))
(struct player (health agility strength))
(struct monster (health) #:mutable)
(struct orc monster (club-level))
(struct hydra monster ())
(struct slime-mold monster (sliminess))
(struct brigand monster ())

(define (orc-battle st)
  (cond 
    [(player-dead? (state-player st))
     (printf "~nYou lost the battle.~n")
     #f]
    [(monsters-dead? (state-monsters st))
     (printf "~nCongratulatuions! You killed all the monsters.~n")
     #t]
    [else (orc-battle (perform-round st))]))

(define (sum-health monsters)
  (foldl (lambda (mstr so-far)
           (+ (monster-health mstr) so-far))
         0
         monsters))

(define (player-dead? player)
  (<= (player-health player) 0))

(define (monsters-dead? monsters)
  (andmap monster-dead? monsters))

(define (monster-dead? mstr)
  (<= (monster-health mstr) 0))

(define (perform-round st)
  (monster-turn (player-turn st)))

(define (player-turn st)
  (printf "~n~nPLAYER'S TURN~n~n")
  (define player (state-player st))
  (show-player player)
  (define new-monsters
    (let attack ([agility (player-agility player)]
                 [monsters (state-monsters st)])
      (show-monsters monsters)
      (define new-monsters (player-attack st))
      (when (and (>= agility 15) 
                 (not (monsters-dead? new-monsters)))
        (attack (- agility 15) new-monsters))
      new-monsters))
  (state player new-monsters))

(define (player-attack st)
  (define strength (player-strength (state-player st)))
  (define monsters (state-monsters st))
  (newline)
  (display "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (case (read)
    ['s (monster-hit (pick-monster monsters)
                     (+ 2 (random (arithmetic-shift strength -1))))]
    ['d (define damage (add1 (random (add1 (truncate (/ strength 6))))))
        (printf "Your double swing has a strength of ~a~n" damage)
        (monster-hit (pick-monster monsters) damage)
        (unless (monsters-dead? monsters)
          (monster-hit (pick-monster monsters) damage))]
    [else (define damage (add1 (random (add1 (truncate (/ strength 3))))))
          (printf "Your roundhouse has a strength of ~a~n" damage)
          (for ([i (in-range damage)])
            (unless (monsters-dead? monsters)
              (monster-hit (random-monster monsters) 1)))])
  monsters)

(define (monster-hit mstr damage)
  (set-monster-health! mstr (- (monster-health mstr) damage))
  (printf "You dealt ~a damage" damage)
  (cond
    [(orc? mstr)
     (if (monster-dead? mstr)
         (printf ", killing the orc!")
         (printf " to the orc!"))]
    [(hydra? mstr)
     (if (monster-dead? mstr)
         (printf ", killing the hydra!")
         (printf ", lopping off ~a of the hydra's heads!" damage))]
    [(slime-mold? mstr)
     (if (monster-dead? mstr)
         (printf ", killing the slime mold!")
         (printf " to the slime mold!"))]
    [(brigand? mstr)
     (if (monster-dead? mstr)
         (printf ", killing the brigand!")
         (printf " to the brigand!"))])
  (newline))

(define (random-monster monsters)
  (define mstr (list-ref monsters (random (length monsters))))
  (if (monster-dead? mstr)
      (random-monster monsters)
      mstr))

(define (pick-monster monsters)
  (printf "Pick a monster: ")
  (define mstr (list-ref monsters (sub1 (read))))
  (if (monster-dead? mstr)
      (begin (printf "That monster is dead. Choose another: ")
             (pick-monster monsters))
      mstr))

(define (monster-turn st)
  (printf "~n~nMONSTER'S TURN~n~n")
  (define new-player
    (foldl (lambda (mstr plr) 
             (if (not (monster-dead? mstr))
                 (monster-attack mstr plr)
                 plr))
           (state-player st)
           (state-monsters st)))
  (state new-player (state-monsters st)))

(define (monster-attack mstr plr)
  (define new-player
    (match mstr
      [(orc health club-level)
       (define damage (random (orc-club-level mstr)))
       (printf "An orc knocks off ~a health." damage)
       (struct-copy player plr [health (- (player-health plr) damage)])]
      [(hydra health)
       (define damage (random (max (arithmetic-shift (monster-health mstr) -1) 2)))
       (printf "A hydra attacks with ~a of its heads. It also grows another one back!" damage)
       (set-monster-health! mstr (add1 (monster-health mstr)))
       (struct-copy player plr [health (- (player-health plr) damage)])]
      [(slime-mold health sliminess)
       (define slime (random (slime-mold-sliminess mstr)))
       (printf "A slime mold wraps around your leg and decreases your agility by ~a" slime)
       (if (zero? (random 2))
         (begin 
           (printf "It also squirts in your face, taking away a health point!")
           (struct-copy player plr 
                        [health (sub1 (player-health plr))]
                        [agility (- (player-agility plr) slime)]))
         (struct-copy player plr [agility (- (player-agility plr) slime)]))]
      [(brigand health)
       (define x (max (player-health plr) (player-agility plr) (player-strength plr)))
       (cond
         [(= x (player-health plr))
          (printf "A brigand shoots you with his slingshot, taking off 2 health points")
          (struct-copy player plr [health (- (player-health plr) 2)])]
         [(= x (player-agility plr))
          (printf "A brigand cuts your leg with his whip, taking off 2 agility points")
          (struct-copy player plr [agility (- (player-agility plr) 2)])]
         [(= x (player-strength plr))
          (printf "A brigand cuts your arm with his whip, taking off 2 strength points")
          (struct-copy player plr [strength (- (player-strength plr) 2)])]
         [else (printf "Didn't match!")])]))
  (printf "...~n")
  new-player)

(define (show-player plr)
  (printf "Player Stats:~nHealth: ~a~nAgility: ~a~nStrength: ~a~n~n"
          (player-health plr)
          (player-agility plr)
          (player-strength plr)))

(define (show-monsters monsters)
  (printf "Your foes:~n")
  (for ([i (in-range (length monsters))]
        [mstr monsters])
    (show-monster mstr (add1 i))))

(define (show-monster mstr monster-num)
  (printf "~a. " monster-num)
  (if (monster-dead? mstr)
      (printf "**DEAD**~n")
      (match mstr
        [(orc health club-level)
         (printf "(Health: ~a) A wicked ORC with a level ~a club.~n"
                 health club-level)]
        [(hydra health)
         (printf "(Health: ~a) A malicious HYDRA with ~a heads.~n"
                 health health)]
        [(slime-mold health sliminess)
         (printf "(Health: ~a) A SLIME MOLD with a sliminess of ~a.~n"
                 health sliminess)]
        [(brigand health)
         (printf "(Health: ~a) A fierce BRIGAND.~n"
                 health)]
        [(monster health)
         (printf "(Health: ~a) A generic MONSTER.~n"
                 health)])))

(define (generate-random-monsters num)
  (define creators
    (list (lambda () (orc (add1 (random 10)) (add1 (random 8))))
          (lambda () (hydra (add1 (random 10))))
          (lambda () (slime-mold (add1 (random 10)) (add1 (random 5))))
          (lambda () (brigand (add1 (random 10))))))
  (for/list ([i (in-range num)])
    ((list-ref creators (random 4)))))

(define (upgrade-player plr points)
  (printf "You have ~a points available.~n" points)
  (printf "Would you like to upgrade [h]ealth, [a]gility, or [s]trength? ")
  (define attribute (read))
  (printf "How many points do you want to apply? ")
  (define pts (read))
  (when (> pts points)
    (printf "You don't have that many")
    (upgrade-player plr points))
  (define new-player
    (case attribute
      ['h (struct-copy player plr [health (+ (player-health plr) pts)])]
      ['a (struct-copy player plr [agility (+ (player-agility plr) pts)])]
      [else (struct-copy player plr [strength (+ (player-strength plr) pts)])]))
  (if (<= (- points pts) 0)
      new-player
      (upgrade-player new-player (- points pts))))

(define (start-game)
  (let loop ([plr (player 10 10 10)])
    (show-player plr)
    (printf "How many monsters do you want to fight (q to quit)? ")
    (define cmd (read))
    (cond 
      [(eq? cmd 'q)
       (printf "You ended with player stats of:~n")
       (show-player plr)]
      [else
       (define mstrs (generate-random-monsters cmd))
       (define points (max (truncate (/ (sum-health mstrs) 3)) 1))
       (define won? (orc-battle (state plr mstrs)))
       (loop (if won?
                 (upgrade-player plr points)
                 plr))])))