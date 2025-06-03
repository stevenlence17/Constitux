(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_VOTING_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_FORK_NOT_FOUND (err u106))
(define-constant ERR_INVALID_THRESHOLD (err u107))

(define-data-var proposal-counter uint u0)
(define-data-var fork-counter uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var voting-period uint u1440)

(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20),
    voting-threshold: uint,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    fork-id: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map forks
  uint
  {
    creator: principal,
    name: (string-ascii 50),
    description: (string-ascii 300),
    parent-fork: (optional uint),
    voting-threshold: uint,
    min-stake: uint,
    created-at: uint,
    active: bool
  }
)

(define-map fork-members
  { fork-id: uint, member: principal }
  { stake: uint, joined-at: uint, voting-power: uint }
)

(define-map user-stakes
  principal
  uint
)

(define-public (create-genesis-fork (name (string-ascii 50)) (description (string-ascii 300)) (voting-threshold uint))
  (let
    (
      (fork-id (+ (var-get fork-counter) u1))
    )
    (asserts! (and (>= voting-threshold u1) (<= voting-threshold u100)) ERR_INVALID_THRESHOLD)
    (map-set forks fork-id {
      creator: tx-sender,
      name: name,
      description: description,
      parent-fork: none,
      voting-threshold: voting-threshold,
      min-stake: (var-get min-proposal-stake),
      created-at: stacks-block-height,
      active: true
    })
    (map-set fork-members { fork-id: fork-id, member: tx-sender } {
      stake: u0,
      joined-at: stacks-block-height,
      voting-power: u100
    })
    (var-set fork-counter fork-id)
    (ok fork-id)
  )
)

(define-public (fork-governance (parent-fork-id uint) (name (string-ascii 50)) (description (string-ascii 300)) (voting-threshold uint))
  (let
    (
      (fork-id (+ (var-get fork-counter) u1))
      (parent-fork (unwrap! (map-get? forks parent-fork-id) ERR_FORK_NOT_FOUND))
    )
    (asserts! (and (>= voting-threshold u1) (<= voting-threshold u100)) ERR_INVALID_THRESHOLD)
    (asserts! (get active parent-fork) ERR_FORK_NOT_FOUND)
    (map-set forks fork-id {
      creator: tx-sender,
      name: name,
      description: description,
      parent-fork: (some parent-fork-id),
      voting-threshold: voting-threshold,
      min-stake: (get min-stake parent-fork),
      created-at: stacks-block-height,
      active: true
    })
    (map-set fork-members { fork-id: fork-id, member: tx-sender } {
      stake: u0,
      joined-at: stacks-block-height,
      voting-power: u100
    })
    (var-set fork-counter fork-id)
    (ok fork-id)
  )
)

(define-public (join-fork (fork-id uint) (stake-amount uint))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (current-stake (default-to u0 (map-get? user-stakes tx-sender)))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (>= stake-amount (get min-stake fork-data)) ERR_INSUFFICIENT_STAKE)
    (map-set user-stakes tx-sender (+ current-stake stake-amount))
    (map-set fork-members { fork-id: fork-id, member: tx-sender } {
      stake: stake-amount,
      joined-at: stacks-block-height,
      voting-power: (/ (* stake-amount u100) (get min-stake fork-data))
    })
    (ok true)
  )
)

(define-public (create-proposal (fork-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type (string-ascii 20)))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (>= (get stake member-data) (get min-stake fork-data)) ERR_INSUFFICIENT_STAKE)
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      proposal-type: proposal-type,
      voting-threshold: (get voting-threshold fork-data),
      votes-for: u0,
      votes-against: u0,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height (var-get voting-period)),
      executed: false,
      fork-id: fork-id
    })
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (fork-id (get fork-id proposal))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (voting-power (get voting-power member-data))
    )
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    (asserts! (<= stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
    (asserts! (>= stacks-block-height (get start-block proposal)) ERR_VOTING_ENDED)
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } {
      vote: vote-for,
      weight: voting-power
    })
    (if vote-for
      (map-set proposals proposal-id (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) }))
      (map-set proposals proposal-id (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) }))
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (approval-rate (if (> total-votes u0) (/ (* (get votes-for proposal) u100) total-votes) u0))
    )
    (asserts! (> stacks-block-height (get end-block proposal)) ERR_VOTING_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_VOTING_ENDED)
    (asserts! (>= approval-rate (get voting-threshold proposal)) ERR_NOT_AUTHORIZED)
    (map-set proposals proposal-id (merge proposal { executed: true }))
    (ok true)
  )
)

(define-public (update-fork-settings (fork-id uint) (new-threshold uint) (new-min-stake uint))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator fork-data)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= new-threshold u1) (<= new-threshold u100)) ERR_INVALID_THRESHOLD)
    (map-set forks fork-id (merge fork-data {
      voting-threshold: new-threshold,
      min-stake: new-min-stake
    }))
    (ok true)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-fork (fork-id uint))
  (map-get? forks fork-id)
)

(define-read-only (get-fork-member (fork-id uint) (member principal))
  (map-get? fork-members { fork-id: fork-id, member: member })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-user-stake (user principal))
  (default-to u0 (map-get? user-stakes user))
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let
      (
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-rate (if (> total-votes u0) (/ (* (get votes-for proposal) u100) total-votes) u0))
        (is-active (and (<= stacks-block-height (get end-block proposal)) (>= stacks-block-height (get start-block proposal))))
        (is-passed (>= approval-rate (get voting-threshold proposal)))
      )
      (ok {
        total-votes: total-votes,
        approval-rate: approval-rate,
        is-active: is-active,
        is-passed: is-passed,
        executed: (get executed proposal)
      })
    )
    ERR_PROPOSAL_NOT_FOUND
  )
)

(define-read-only (get-fork-stats (fork-id uint))
  (match (map-get? forks fork-id)
    fork-data
    (ok {
      name: (get name fork-data),
      member-count: u0,
      total-stake: u0,
      active: (get active fork-data),
      created-at: (get created-at fork-data)
    })
    ERR_FORK_NOT_FOUND
  )
)