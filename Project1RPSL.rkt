#lang racket

;; Name: Jayden Cruz

;; Date: 09/29/2024

;; Class: CSC 3110

;; Pledge: I have neither given nor received unauthorized aid on this program.

;; Description: A Rock-Paper-Scissors-Lizard-Spock game between two players.

;; Input: Player names, number of points to win, and gesture choices.

;; Output: Round results, score updates, and the final winner.

(define gestures '((0 . "Rock") 
                   (1 . "Paper") 
                   (2 . "Scissors") 
                   (3 . "Lizard") 
                   (4 . "Spock")))

;; Welcome message
(define (print-welcome)
  (display "Welcome to Rock, Paper, Scissors, Lizard, Spock!\n"))

;; Function to print the game rules
(define (print-rules)
  (display "\nGame Rules:\n")
  (display "0: Rock crushes Scissors, crushes Lizard\n")
  (display "1: Paper covers Rock, disproves Spock\n")
  (display "2: Scissors cuts Paper, decapitates Lizard\n")
  (display "3: Lizard poisons Spock, eats Paper\n")
  (display "4: Spock vaporizes Rock, smashes Scissors\n"))

;; Function to get the name of a gesture
(define (get-gesture-name gesture)
  (cdr (assoc gesture gestures)))

;; Determine the winner of a round
(define (determine-winner player1-gesture player2-gesture)
  (cond
    ((equal? player1-gesture player2-gesture) 0) ;; Tie
    ((or (and (= player1-gesture 0) (member player2-gesture '(2 3))) ;; Rock beats Scissors, Lizard
         (and (= player1-gesture 1) (member player2-gesture '(0 4))) ;; Paper beats Rock, Spock
         (and (= player1-gesture 2) (member player2-gesture '(1 3))) ;; Scissors beats Paper, Lizard
         (and (= player1-gesture 3) (member player2-gesture '(1 4))) ;; Lizard beats Paper, Spock
         (and (= player1-gesture 4) (member player2-gesture '(0 2)))) ;; Spock beats Rock, Scissors
     1) ;; Player 1 wins
    (else 2))) ;; Player 2 wins

;; Function to get a valid gesture input from the user
(define (get-valid-gesture player-name)
  (let ((gesture (read)))
    (if (and (number? gesture) (>= gesture 0) (<= gesture 4))
        gesture
        (begin
          (display (string-append player-name ", enter a valid gesture (0-4): "))
          (get-valid-gesture player-name)))))

;; Main game loop
(define (play-game)
  (print-welcome)
  (print-rules)

  ;; Get player names
  (display "Please enter Player 1's name: ")
  (define player1 (symbol->string (read)))
  (display "Please enter Player 2's name: ")
  (define player2 (symbol->string (read)))

  ;; Get the number of points to win
  (display "How many points does it take to win? ")
  (define points-to-win (read))

  ;; Validate the points to win
  (if (<= points-to-win 0)
      (begin
        (display "Please enter a valid number of points to win.\n")
        (play-game))
      (let loop ((player1-score 0)
                 (player2-score 0)
                 (round 1))
        (display (string-append "\nROUND " (number->string round) "\n"))
        (display (string-append player1 ", enter your throw (0: Rock, 1: Paper, 2: Scissors, 3: Lizard, 4: Spock): "))
        (define player1-gesture (get-valid-gesture player1))
        (display (string-append player2 ", enter your throw (0: Rock, 1: Paper, 2: Scissors, 3: Lizard, 4: Spock): "))
        (define player2-gesture (get-valid-gesture player2))

        (define winner (determine-winner player1-gesture player2-gesture))

        ;; Display round result
        (cond
          ((= winner 0) 
           (display "It's a tie! Both players will throw again.\n")
           (loop player1-score player2-score round)) ;; Restart round on a tie
          ((= winner 1) 
           (display (string-append (get-gesture-name player1-gesture) " defeats " (get-gesture-name player2-gesture) "\n"))
           (display (string-append player1 " wins the round!\n"))
           (if (>= (+ player1-score 1) points-to-win)
               (display (string-append "\n" player1 " wins the game!\n"))
               (loop (+ player1-score 1) player2-score (+ round 1))))
          ((= winner 2) 
           (display (string-append (get-gesture-name player2-gesture) " defeats " (get-gesture-name player1-gesture) "\n"))
           (display (string-append player2 " wins the round!\n"))
           (if (>= (+ player2-score 1) points-to-win)
               (display (string-append "\n" player2 " wins the game!\n"))
               (loop player1-score (+ player2-score 1) (+ round 1))))))))

;; Start the game
(play-game)
