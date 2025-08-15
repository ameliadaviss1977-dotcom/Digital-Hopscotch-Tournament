;; Digital Hopscotch Tournament
;; Core tournament system with game logging, skill divisions, and bracket management

;; Error codes
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-score (err u101))
(define-constant err-game-not-found (err u102))
(define-constant err-division-not-found (err u103))
(define-constant err-tournament-full (err u104))

;; Tournament constants
(define-constant max-score u1000)
(define-constant max-players-per-division u16)

;; Data structures
(define-map games
  { game-id: uint }
  {
    player1: principal,
    player2: principal,
    score1: uint,
    score2: uint,
    winner: principal,
    division: uint,
    round: uint,
    block-height: uint
  }
)

(define-map divisions
  { division-id: uint }
  {
    name: (string-ascii 32),
    min-skill: uint,
    max-skill: uint,
    player-count: uint,
    active: bool
  }
)

(define-map player-stats
  { player: principal }
  {
    total-games: uint,
    wins: uint,
    losses: uint,
    skill-level: uint,
    current-division: uint
  }
)

(define-map tournament-brackets
  { division: uint, round: uint, position: uint }
  {
    player1: (optional principal),
    player2: (optional principal),
    winner: (optional principal),
    game-id: (optional uint)
  }
)

;; Data vars
(define-data-var game-counter uint u0)
(define-data-var tournament-admin principal tx-sender)

;; Initialize divisions
(map-set divisions { division-id: u1 } { name: "Beginner", min-skill: u0, max-skill: u300, player-count: u0, active: true })
(map-set divisions { division-id: u2 } { name: "Intermediate", min-skill: u301, max-skill: u700, player-count: u0, active: true })
(map-set divisions { division-id: u3 } { name: "Advanced", min-skill: u701, max-skill: u1000, player-count: u0, active: true })

;; Public functions

;; Log a completed game
(define-public (log-game (player1 principal) (player2 principal) (score1 uint) (score2 uint) (division uint))
  (let ((game-id (+ (var-get game-counter) u1))
        (winner (if (> score1 score2) player1 player2)))
    (asserts! (and (<= score1 max-score) (<= score2 max-score)) err-invalid-score)
    (asserts! (is-some (map-get? divisions { division-id: division })) err-division-not-found)

    (map-set games { game-id: game-id }
      {
        player1: player1,
        player2: player2,
        score1: score1,
        score2: score2,
        winner: winner,
        division: division,
        round: u0,
        block-height: stacks-block-height
      })

    (update-player-stats player1 (is-eq winner player1))
    (update-player-stats player2 (is-eq winner player2))
    (var-set game-counter game-id)
    (ok game-id)
  )
)

;; Register player for tournament division
(define-public (register-for-division (division uint))
  (let ((div-info (unwrap! (map-get? divisions { division-id: division }) err-division-not-found))
        (player-info (default-to { total-games: u0, wins: u0, losses: u0, skill-level: u0, current-division: u0 }
                                 (map-get? player-stats { player: tx-sender }))))
    (asserts! (get active div-info) err-division-not-found)
    (asserts! (< (get player-count div-info) max-players-per-division) err-tournament-full)
    (asserts! (and (>= (get skill-level player-info) (get min-skill div-info))
                   (<= (get skill-level player-info) (get max-skill div-info))) err-unauthorized)

    (map-set divisions { division-id: division }
      (merge div-info { player-count: (+ (get player-count div-info) u1) }))

    (map-set player-stats { player: tx-sender }
      (merge player-info { current-division: division }))
    (ok true)
  )
)

;; Create bracket match
(define-public (create-bracket-match (division uint) (round uint) (position uint)
                                    (player1 principal) (player2 principal))
  (begin
    (asserts! (is-eq tx-sender (var-get tournament-admin)) err-unauthorized)
    (map-set tournament-brackets { division: division, round: round, position: position }
      {
        player1: (some player1),
        player2: (some player2),
        winner: none,
        game-id: none
      })
    (ok true)
  )
)

;; Record bracket game result
(define-public (record-bracket-result (division uint) (round uint) (position uint)
                                     (score1 uint) (score2 uint))
  (let ((bracket-match (unwrap! (map-get? tournament-brackets { division: division, round: round, position: position }) err-game-not-found))
        (player1 (unwrap! (get player1 bracket-match) err-game-not-found))
        (player2 (unwrap! (get player2 bracket-match) err-game-not-found))
        (winner (if (> score1 score2) player1 player2)))

    (asserts! (or (is-eq tx-sender player1) (is-eq tx-sender player2)) err-unauthorized)

    (let ((game-id (unwrap! (log-game player1 player2 score1 score2 division) err-invalid-score)))
      (map-set tournament-brackets { division: division, round: round, position: position }
        (merge bracket-match { winner: (some winner), game-id: (some game-id) }))
      (ok game-id)
    )
  )
)

;; Read-only functions

(define-read-only (get-game (game-id uint))
  (map-get? games { game-id: game-id })
)

(define-read-only (get-player-stats (player principal))
  (map-get? player-stats { player: player })
)

(define-read-only (get-division-info (division-id uint))
  (map-get? divisions { division-id: division-id })
)

(define-read-only (get-bracket-match (division uint) (round uint) (position uint))
  (map-get? tournament-brackets { division: division, round: round, position: position })
)

;; Private functions

(define-private (update-player-stats (player principal) (won bool))
  (let ((current-stats (default-to { total-games: u0, wins: u0, losses: u0, skill-level: u0, current-division: u0 }
                                  (map-get? player-stats { player: player })))
        (new-wins (if won (+ (get wins current-stats) u1) (get wins current-stats)))
        (new-losses (if won (get losses current-stats) (+ (get losses current-stats) u1)))
        (new-total (+ (get total-games current-stats) u1))
        (new-skill (calculate-skill-level new-wins new-total)))

    (map-set player-stats { player: player }
      (merge current-stats {
        total-games: new-total,
        wins: new-wins,
        losses: new-losses,
        skill-level: new-skill
      }))
  )
)

(define-private (calculate-skill-level (wins uint) (total-games uint))
  (if (is-eq total-games u0)
    u0
    (/ (* wins u1000) total-games)
  )
)
