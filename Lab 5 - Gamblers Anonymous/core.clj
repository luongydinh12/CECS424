(ns cards.core
  (:require [clojure.pprint :refer [pprint]]))

;; When debugging, you can use (pprint x) to do a "pretty print" of the variable x,
;; instead of x being printed on a single line.

;; A card is a kind (integer 1-13) and suit (string)
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
  ;; TODO: replace the following line with logic that converts the card's kind to a string.
  ;; Reminder: a 1 means "Ace", 11 means "Jack", 12 means "Queen", 13 means "King".
  ;; Any other kind should be converted directly to a string, e.g., 2 becomes "2".
  ;; The str function can convert an integer to a string.
  ;; The card-value function below is a hint.

  (let [kind (str (kind card))

        ;; TODO: then do the same thing for the card's suit. 0 = "Spades", 1 = "Clubs",
        ;; 2 = "Diamonds", 3 = "Hearts"
        suit (str (suit card))]

    ;; Returns a string of the form "[kind] of [suit]"
    (str kind " of " card)))

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
  (let [;; TODO: modify the next line to sum the card values of each card in the hand.
        ;; HINT: map and reduce
        sum 0
        ;; TODO: modify the next line to count the number of aces in the hand.
        ;; HINT: filter and count
        num-aces 0]
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
  ;; TODO: take the top (first) card from the game state's deck and cons it onto the hand
  ;; for the given owner. Return the new game state, including a new deck with the top
  ;; card removed.

  ;; TODO: this is just so the code compiles; fix it.
  game-state)

;; Given a game state, takes the dealer's turn and returns a new game state after the 
;; dealer has acted.
(defn dealer-turn [game-state]
  ;; Get the dealer's hand and total score.
  (let [dealer (hand game-state :dealer)
        score (hand-total dealer)]

    ;; TODO: the following line prints the cards in the dealer's hand, but with ugly output
    ;; because Clojure doesn't know how to format a card variable for output. Transform each card
    ;; in the hand to a string using card-str.
    (println (str "Dealer's hand: "
                  dealer
                  "; "
                  score
                  " points."))

    ;; Dealer rules: must hit if score < 17
    (cond
      (> score 21)
      ;; do allows us to have more than one statement in a branch.
      (do (println "Dealer busts!")
          game-state)

      (< score 17)
      (do (println "Dealer hits")
          ;; the game state is changed; the result of "hit" is the new state.
          ;; the dealer gets to take another action using the new state.
          (dealer-turn (hit game-state :dealer)))

      :else
      (do (println "Dealer must stay")
          game-state))))

;; Given a game state and a strategy, takes the player's entire turn by recursively applying
;; the strategy until the strategy decides to stay (not hit). Returns the new game state
;; after the player's turn is complete.
(defn player-turn [game-state player-strategy]
  ;; TODO: code this method using dealer-turn as a guide. Follow the same standard
  ;; of printing output. The function must reutrn the new game state after the player's action has finished.

  ;; Unlike the dealer, the player gets to make choices about whether they will hit or stay.
  ;; The (< score 17) branch from dealer-turn is inappropriate; in its place, we will allow a
  ;; "strategy" to decide whether to hit. A strategy is a function that accepts the current
  ;; game state and returns true if the player should hit, and false otherwise.
  ;; player-turn must call the player-strategy function to decide whether to hit or stay.

  ;; TODO: the next line is just so the code compiles; remove it when you are done.
  game-state)


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
  ;; TODO: replace the 0 on the next line with the card-str of the dealer's first card.
  (println "Dealer is showing: " 0)

  ;; TODO: play the game! First the player gets their turn. The dealer then takes their turn,
  ;; using the state of the game after the player's turn finished.



  ;; TODO: determine the winner! Get the hand scores for the dealer and the player.
  ;; The player wins if they did not bust (score <= 21) AND EITHER:
  ;;     - the dealer busts; OR
  ;;     - the player's score > dealer's score
  ;; If neither side busts and they have the same score, the result is a "draw".

  ;; Return a game log object with a value of 1 for the correct winner.

  ;; TODO: this is a "blank" game log. Return something more appropriate for each of the
  ;; outcomes described above.
  (make-log 0 0 0))

;; Plays n games of blackjack with the given player strategy. Returns a game log
;; summarizing the number of wins, losses, and draws.
(defn many-games [n player-strategy]
  (letfn [;; This defines an inner helper function for doing the tail recursion.
          (many-games-tail [n player-strategy accumulated-log]
            ;; TODO: create a new game using new-game.
            ;; Play that game using one-game.
            ;; Take the result of that game and add it to the accumulated-log, using add-logs.
            ;; If this is the last game (n == 1), then the combined log is the answer.
            ;; Otherwise, the combined log becomes the new accumulated-log in a recursive call to
            ;; many-games-tail, with n reduced by 1.
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



(defn -main [& args]
  (pprint (many-games 1 interactive-player-strategy)))

(-main)