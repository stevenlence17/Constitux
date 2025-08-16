(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_VOTING_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_FORK_NOT_FOUND (err u106))
(define-constant ERR_INVALID_THRESHOLD (err u107))
(define-constant ERR_DELEGATION_NOT_FOUND (err u108))
(define-constant ERR_SELF_DELEGATION (err u109))
(define-constant ERR_CIRCULAR_DELEGATION (err u110))
(define-constant ERR_DELEGATE_NOT_MEMBER (err u111))
(define-constant ERR_DELEGATION_EXISTS (err u112))
(define-constant ERR_INSUFFICIENT_TREASURY_FUNDS (err u113))
(define-constant ERR_TREASURY_NOT_FOUND (err u114))
(define-constant ERR_INVALID_AMOUNT (err u115))
(define-constant ERR_WITHDRAWAL_NOT_APPROVED (err u116))
(define-constant ERR_PROPOSAL_NOT_FUNDED (err u117))
(define-constant ERR_TREASURY_LOCKED (err u118))

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

(define-map delegations
  { fork-id: uint, delegator: principal }
  { delegate: principal, delegated-at: uint, active: bool }
)

(define-map delegate-powers
  { fork-id: uint, delegate: principal }
  { total-delegated-power: uint, delegator-count: uint }
)

(define-map delegation-chains
  { fork-id: uint, original-delegator: principal }
  { final-delegate: principal, chain-length: uint }
)

(define-map fork-treasuries
  uint
  { 
    balance: uint,
    total-contributions: uint,
    total-withdrawals: uint,
    locked: bool,
    created-at: uint
  }
)

(define-map treasury-contributions
  { fork-id: uint, contributor: principal }
  { total-contributed: uint, last-contribution: uint, contribution-count: uint }
)

(define-map funded-proposals
  uint
  {
    funding-amount: uint,
    funded-at: uint,
    funded-by-fork: uint,
    funding-approved: bool
  }
)

(define-map treasury-withdrawals
  uint
  {
    fork-id: uint,
    amount: uint,
    recipient: principal,
    purpose: (string-ascii 100),
    requested-at: uint,
    approved: bool,
    executed: bool
  }
)

(define-data-var withdrawal-counter uint u0)

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
    (map-set fork-treasuries fork-id {
      balance: u0,
      total-contributions: u0,
      total-withdrawals: u0,
      locked: false,
      created-at: stacks-block-height
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
    (map-set fork-treasuries fork-id {
      balance: u0,
      total-contributions: u0,
      total-withdrawals: u0,
      locked: false,
      created-at: stacks-block-height
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

(define-private (get-effective-voting-power (fork-id uint) (voter principal))
  (let
    (
      (member-data (map-get? fork-members { fork-id: fork-id, member: voter }))
      (delegate-power (default-to { total-delegated-power: u0, delegator-count: u0 } 
                      (map-get? delegate-powers { fork-id: fork-id, delegate: voter })))
    )
    (match member-data
      member
      (+ (get voting-power member) (get total-delegated-power delegate-power))
      u0
    )
  )
)

(define-private (get-direct-delegate (fork-id uint) (delegator principal))
  (match (map-get? delegations { fork-id: fork-id, delegator: delegator })
    delegation
    (if (get active delegation)
      (get delegate delegation)
      delegator
    )
    delegator
  )
)

(define-public (delegate-voting-power (fork-id uint) (delegate principal))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (delegator-member (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (delegate-member (unwrap! (map-get? fork-members { fork-id: fork-id, member: delegate }) ERR_DELEGATE_NOT_MEMBER))
      (existing-delegation (map-get? delegations { fork-id: fork-id, delegator: tx-sender }))
      (delegator-power (get voting-power delegator-member))
      (current-delegate-power (default-to { total-delegated-power: u0, delegator-count: u0 } 
                              (map-get? delegate-powers { fork-id: fork-id, delegate: delegate })))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (not (is-eq tx-sender delegate)) ERR_SELF_DELEGATION)
    (asserts! (is-none existing-delegation) ERR_DELEGATION_EXISTS)

    (map-set delegations { fork-id: fork-id, delegator: tx-sender } {
      delegate: delegate,
      delegated-at: stacks-block-height,
      active: true
    })
    (map-set delegate-powers { fork-id: fork-id, delegate: delegate } {
      total-delegated-power: (+ (get total-delegated-power current-delegate-power) delegator-power),
      delegator-count: (+ (get delegator-count current-delegate-power) u1)
    })
    (map-set delegation-chains { fork-id: fork-id, original-delegator: tx-sender } {
      final-delegate: delegate,
      chain-length: u1
    })
    (ok true)
  )
)

(define-public (revoke-delegation (fork-id uint))
  (let
    (
      (delegation (unwrap! (map-get? delegations { fork-id: fork-id, delegator: tx-sender }) ERR_DELEGATION_NOT_FOUND))
      (delegate (get delegate delegation))
      (delegator-member (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (delegator-power (get voting-power delegator-member))
      (current-delegate-power (unwrap! (map-get? delegate-powers { fork-id: fork-id, delegate: delegate }) ERR_DELEGATION_NOT_FOUND))
    )
    (asserts! (get active delegation) ERR_DELEGATION_NOT_FOUND)
    (map-set delegations { fork-id: fork-id, delegator: tx-sender } (merge delegation { active: false }))
    (map-set delegate-powers { fork-id: fork-id, delegate: delegate } {
      total-delegated-power: (- (get total-delegated-power current-delegate-power) delegator-power),
      delegator-count: (- (get delegator-count current-delegate-power) u1)
    })
    (map-delete delegation-chains { fork-id: fork-id, original-delegator: tx-sender })
    (ok true)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (fork-id (get fork-id proposal))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (voting-power (get-effective-voting-power fork-id tx-sender))
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

(define-read-only (get-delegation (fork-id uint) (delegator principal))
  (map-get? delegations { fork-id: fork-id, delegator: delegator })
)

(define-read-only (get-delegate-power (fork-id uint) (delegate principal))
  (map-get? delegate-powers { fork-id: fork-id, delegate: delegate })
)

(define-read-only (get-delegation-chain (fork-id uint) (original-delegator principal))
  (map-get? delegation-chains { fork-id: fork-id, original-delegator: original-delegator })
)

(define-read-only (get-effective-power (fork-id uint) (member principal))
  (ok (get-effective-voting-power fork-id member))
)



(define-public (contribute-to-treasury (fork-id uint) (amount uint))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (current-contribution (default-to { total-contributed: u0, last-contribution: u0, contribution-count: u0 }
                            (map-get? treasury-contributions { fork-id: fork-id, contributor: tx-sender })))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (not (get locked treasury)) ERR_TREASURY_LOCKED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set fork-treasuries fork-id (merge treasury {
      balance: (+ (get balance treasury) amount),
      total-contributions: (+ (get total-contributions treasury) amount)
    }))
    (map-set treasury-contributions { fork-id: fork-id, contributor: tx-sender } {
      total-contributed: (+ (get total-contributed current-contribution) amount),
      last-contribution: stacks-block-height,
      contribution-count: (+ (get contribution-count current-contribution) u1)
    })
    (ok true)
  )
)

(define-public (fund-proposal (proposal-id uint) (amount uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (fork-id (get fork-id proposal))
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (not (get locked treasury)) ERR_TREASURY_LOCKED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance treasury) amount) ERR_INSUFFICIENT_TREASURY_FUNDS)
    (asserts! (get executed proposal) ERR_VOTING_ACTIVE)
    (asserts! (is-none (map-get? funded-proposals proposal-id)) ERR_PROPOSAL_NOT_FUNDED)
    (map-set fork-treasuries fork-id (merge treasury {
      balance: (- (get balance treasury) amount),
      total-withdrawals: (+ (get total-withdrawals treasury) amount)
    }))
    (map-set funded-proposals proposal-id {
      funding-amount: amount,
      funded-at: stacks-block-height,
      funded-by-fork: fork-id,
      funding-approved: true
    })
    (try! (as-contract (stx-transfer? amount tx-sender (get proposer proposal))))
    (ok true)
  )
)

(define-public (request-treasury-withdrawal (fork-id uint) (amount uint) (recipient principal) (purpose (string-ascii 100)))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
      (member-data (unwrap! (map-get? fork-members { fork-id: fork-id, member: tx-sender }) ERR_NOT_AUTHORIZED))
      (withdrawal-id (+ (var-get withdrawal-counter) u1))
    )
    (asserts! (get active fork-data) ERR_FORK_NOT_FOUND)
    (asserts! (not (get locked treasury)) ERR_TREASURY_LOCKED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance treasury) amount) ERR_INSUFFICIENT_TREASURY_FUNDS)
    (map-set treasury-withdrawals withdrawal-id {
      fork-id: fork-id,
      amount: amount,
      recipient: recipient,
      purpose: purpose,
      requested-at: stacks-block-height,
      approved: false,
      executed: false
    })
    (var-set withdrawal-counter withdrawal-id)
    (ok withdrawal-id)
  )
)

(define-public (approve-treasury-withdrawal (withdrawal-id uint))
  (let
    (
      (withdrawal (unwrap! (map-get? treasury-withdrawals withdrawal-id) ERR_WITHDRAWAL_NOT_APPROVED))
      (fork-id (get fork-id withdrawal))
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator fork-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get approved withdrawal)) ERR_WITHDRAWAL_NOT_APPROVED)
    (asserts! (not (get executed withdrawal)) ERR_WITHDRAWAL_NOT_APPROVED)
    (asserts! (>= (get balance treasury) (get amount withdrawal)) ERR_INSUFFICIENT_TREASURY_FUNDS)
    (map-set treasury-withdrawals withdrawal-id (merge withdrawal { approved: true }))
    (ok true)
  )
)

(define-public (execute-treasury-withdrawal (withdrawal-id uint))
  (let
    (
      (withdrawal (unwrap! (map-get? treasury-withdrawals withdrawal-id) ERR_WITHDRAWAL_NOT_APPROVED))
      (fork-id (get fork-id withdrawal))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
      (amount (get amount withdrawal))
      (recipient (get recipient withdrawal))
    )
    (asserts! (get approved withdrawal) ERR_WITHDRAWAL_NOT_APPROVED)
    (asserts! (not (get executed withdrawal)) ERR_WITHDRAWAL_NOT_APPROVED)
    (asserts! (>= (get balance treasury) amount) ERR_INSUFFICIENT_TREASURY_FUNDS)
    (map-set fork-treasuries fork-id (merge treasury {
      balance: (- (get balance treasury) amount),
      total-withdrawals: (+ (get total-withdrawals treasury) amount)
    }))
    (map-set treasury-withdrawals withdrawal-id (merge withdrawal { executed: true }))
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)

(define-public (lock-treasury (fork-id uint))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator fork-data)) ERR_NOT_AUTHORIZED)
    (map-set fork-treasuries fork-id (merge treasury { locked: true }))
    (ok true)
  )
)

(define-public (unlock-treasury (fork-id uint))
  (let
    (
      (fork-data (unwrap! (map-get? forks fork-id) ERR_FORK_NOT_FOUND))
      (treasury (unwrap! (map-get? fork-treasuries fork-id) ERR_TREASURY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator fork-data)) ERR_NOT_AUTHORIZED)
    (map-set fork-treasuries fork-id (merge treasury { locked: false }))
    (ok true)
  )
)

(define-read-only (check-delegation-validity (fork-id uint) (delegator principal) (proposed-delegate principal))
  (let
    (
      (delegator-member (map-get? fork-members { fork-id: fork-id, member: delegator }))
      (delegate-member (map-get? fork-members { fork-id: fork-id, member: proposed-delegate }))
      (existing-delegation (map-get? delegations { fork-id: fork-id, delegator: delegator }))
    )
    (ok {
      delegator-is-member: (is-some delegator-member),
      delegate-is-member: (is-some delegate-member),
      no-existing-delegation: (is-none existing-delegation),
      not-self-delegation: (not (is-eq delegator proposed-delegate))
    })
  )
)

(define-read-only (get-treasury (fork-id uint))
  (map-get? fork-treasuries fork-id)
)

(define-read-only (get-treasury-contribution (fork-id uint) (contributor principal))
  (map-get? treasury-contributions { fork-id: fork-id, contributor: contributor })
)

(define-read-only (get-funded-proposal (proposal-id uint))
  (map-get? funded-proposals proposal-id)
)

(define-read-only (get-treasury-withdrawal (withdrawal-id uint))
  (map-get? treasury-withdrawals withdrawal-id)
)

(define-read-only (get-treasury-stats (fork-id uint))
  (match (map-get? fork-treasuries fork-id)
    treasury
    (ok {
      balance: (get balance treasury),
      total-contributions: (get total-contributions treasury),
      total-withdrawals: (get total-withdrawals treasury),
      locked: (get locked treasury),
      net-funds: (- (get total-contributions treasury) (get total-withdrawals treasury))
    })
    ERR_TREASURY_NOT_FOUND
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

