(ns cards.core
  (:require [clojure.pprint :refer [pprint]]))

(defn make-card [kind suit]
  {:kind kind, :suit suit})

;; Selector for a card's Kind.
(defn kind [card]
  (:kind card))

;; Selector for a card's Suit.
(defn suit [card]
  (:suit card))

;; A "tostring" method for cards.
(defn card-str [card]
  (let [kind (str (case (kind card)
                    1 "Ace" 
                    11 "Jack" 
                    12 "Queen"
                    13 "King"
                    (kind card)))

        suit (str (case (suit card)
                    0 "Spades" 
                    1 "Clubs" 
                    2 "Diamonds"
                    3 "Heart"
                    (suit card)))]

    ;; Returns a string of the form "[kind] of [suit]"
    (str kind " of " suit)))

;; Returns the integer value of a card.
(defn card-value [card]
  (case (kind card)
    1 11 ;; ace
    11 10 ;; face cards
    12 10
    13 10
    ;; the final case is the "default"
    (kind card)))

;; Returns the total number of "points" in the hand.
(defn hand-total [hand]
  (let [sum (reduce + 0 (map card-value hand))
        num-aces (count (filter #(= 1 (kind %)) hand))]
    (if (or (<= sum 21) (zero? num-aces))
      sum ;; no adjustment if the sum doesn't exceed 21 or there are no aces
      (let [max-aces (int (Math/ceil (/ (- sum 21) 10)))]
        ;; if we exceed 21, then reduce by 10 points for each ace until we are good
        (- sum (* 10 (min num-aces max-aces)))))))

;; Constructs a new unshuffled deck as a list of cards
(defn make-deck []
  (for [suit (range 0 4)
        kind (range 1 14)]
    (make-card kind suit)))

;; The game state consists of the deck to draw from, the player's hand, and the dealer's hand.
(defn make-state [draw player dealer]
  {:deck draw, :player player, :dealer dealer})

;; Selector for the player's hand.
(defn player-hand [game-state]
  (:player game-state))

;; Selector for the dealer's hand.
(defn dealer-hand [game-state]
  (:dealer game-state))

;; Selector for the deck (draw pile).
(defn deck [game-state]
  (:deck game-state))

;; Given an owner that is either :player or :dealer, selects that owner's hand from the game state.
(defn hand [game-state owner]
  (owner game-state))


;; A new game is started by taking a new deck, shuffling it, giving the first and third
;; card to the player, and the second and fourth to the dealer.
(defn new-game []
  (let [new-deck (make-deck)
        shuffled-deck (shuffle new-deck)
        player-hand (list (first shuffled-deck) (nth shuffled-deck 2))
        dealer-hand (list (second shuffled-deck) (nth shuffled-deck 3))
        draw-pile (drop 4 shuffled-deck)]
    (make-state draw-pile player-hand dealer-hand)))


;; Given a game state and an owner that is either :player or :dealer,
;; deal one card from the deck and add it to the front of the given owner's hand.
;; Return the new game state.
(defn hit [game-state owner]
  (let [ topcard (first (deck game-state))
         newdeck (next (deck game-state))
         player (if (= owner :player)
                    (cons topcard (player-hand game-state))
                    (player-hand game-state)
                )
         dealer (if (= owner :dealer)
                    (cons topcard (dealer-hand game-state))
                    (dealer-hand game-state)
                )
        ]
        (make-state newdeck player dealer )   
  )
)

;; Given a game state, takes the dealer's turn and returns a new game state after the 
;; dealer has acted.
(defn dealer-turn [game-state]
  ;; Get the dealer's hand and total score.
  (let [dealer (hand game-state :dealer)
        score (hand-total dealer)]
    ;; Dealer rules: must hit if score < 17
    (cond
      (> score 21)
      ;; do allows us to have more than one statement in a branch.
      game-state
      (< score 17)
          (dealer-turn (hit game-state :dealer))
      :else
          game-state)))

;; Given a game state and a strategy, takes the player's entire turn by recursively applying
;; the strategy until the strategy decides to stay (not hit). Returns the new game state
;; after the player's turn is complete.
(defn player-turn [game-state player-strategy]
  (let [player (hand game-state :player)
        score (hand-total player)]
    ;; Unlike the dealer, the player gets to make choices about whether they will hit or stay.
    ;; The (< score 17) branch from dealer-turn is inappropriate; in its place, we will allow a
    ;; "strategy" to decide whether to hit. A strategy is a function that accepts the current
    ;; game state and returns true if the player should hit, and false otherwise.
    ;; player-turn must call the player-strategy function to decide whether to hit or stay.                 
    (cond
      (> score 21)   
          game-state
      :else
      (do (if (player-strategy game-state) 
              (player-turn (hit game-state :player) player-strategy) 
              game-state)
          ))))

;; A type for the log of results from many games.
(defn make-log [player-wins dealer-wins draws]
  {:player-wins player-wins,
   :dealer-wins dealer-wins,
   :draws draws})

;; Adds two game log objects into a single sum log.
(defn add-logs [log1 log2]
  (make-log
    (+ (:player-wins log1) (:player-wins log2))
    (+ (:dealer-wins log1) (:dealer-wins log2))
    (+ (:draws log1) (:draws log2))))

;; Plays one game of blackjack in which the player follows the given strategy.
;; Returns a log of the result of the game.
(defn one-game [game-state player-strategy]
  (let [playerhand (player-hand game-state)
        playerscore (hand-total playerhand)
        player-aces (count (filter #(= 1 (kind %)) playerhand))
        dealerhand (dealer-hand game-state)
        dealerscore (hand-total dealerhand)
        dealer-aces (count (filter #(= 1 (kind %)) dealerhand))]
    (cond
      (and (= dealerscore 21) (= 1 dealer-aces))
        (if (and (= playerscore 21) (= 1 player-aces))
            (make-log 0 0 1) ;; draw              
            (make-log 0 1 0) ;; dealer wins
        )
      :else
        (let [playerturn (player-turn game-state player-strategy)
              dealerturn (dealer-turn playerturn)
              playerscore (hand-total (player-hand dealerturn))
              dealerscore (hand-total (dealer-hand dealerturn))]
          (if (and (<= playerscore 21) (or (> dealerscore 21) (> playerscore dealerscore)))
              (make-log 1 0 0)
              (if (= playerscore dealerscore)
                  (make-log 0 0 1)
                  (make-log 0 1 0)
              )
          )      
        )
    )
  )
)

;; Plays n games of blackjack with the given player strategy. Returns a game log
;; summarizing the number of wins, losses, and draws.
(defn many-games [n player-strategy]
  (letfn [;; This defines an inner helper function for doing the tail recursion.
          (many-games-tail [n player-strategy accumulated-log]
            (let [newgame (one-game (new-game) player-strategy)
                  newlog (add-logs newgame accumulated-log)]
              (if (= n 1) 
                  newlog
                  (many-games-tail (- n 1) player-strategy newlog)
              )            
            )
            )]
    ;; Start the tail recursion with a blank accumulated-log.
    (many-games-tail n player-strategy (make-log 0 0 0))))


;;; Player strategies.

;; The interactive strategy asks the user if they want to hit.
(defn interactive-player-strategy [game-state]
  (println "(h)it or (s)tay?")
  (flush)
  (let [input (read-line)]
    (= "h" input))) ;; return true if the user enters "h", false otherwise.

(defn inactive-player-strategy [game-state]
  false)

(defn greedy-player-strategy [game-state]
(if (< (hand-total (player-hand game-state)) 21)
    true
    false))
    
(defn coin-flip-player-strategy [game-state]
  (if (= 1 (rand-int 2))
      true
      false
  )
)

(defn basic-player-strategy [game-state]
  (let [ dealerfirstcard (kind (first (hand game-state :dealer)))
         playertotal (hand-total (player-hand game-state))
         num-aces (count (filter #(= 1 (kind %)) (player-hand game-state)))]
      (cond
        (and (>= dealerfirstcard 2) (<= dealerfirstcard 6))
          (cond 
            (>= playertotal 12)
              false
            :else
              true
          )
      
        (and (>= dealerfirstcard 7) (<= dealerfirstcard 13))
          (if (<= playertotal 16)
              true
              false
          )      
      
        (= 1 dealerfirstcard)
          (if (and (<= playertotal 16) (> num-aces 0))
              true
              (if (<= playertotal 11)
                  true
                  false
              )
          )
      )
  )
)

(defn -main [& args]
  (println "inactive-player-strategy")
  (pprint (many-games 1000 inactive-player-strategy))
  (println )
  (println "greedy-player-strategy")
  (pprint (many-games 1000 greedy-player-strategy))
  (println )
  (println "coin-flip-player-strategy")
  (pprint (many-games 1000 coin-flip-player-strategy))
  (println )
  (println "basic-player-strategy")
  (pprint (many-games 1000 basic-player-strategy))
)

(-main)

