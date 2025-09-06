;; Fork Reputation System
;; Tracks and scores governance quality based on activity, proposals, and member engagement

(define-constant ERR_FORK_NOT_FOUND (err u200))
(define-constant ERR_UNAUTHORIZED (err u201))
(define-constant ERR_INVALID_SCORE (err u202))
(define-constant ERR_REPUTATION_EXISTS (err u203))
(define-constant ERR_INSUFFICIENT_ACTIVITY (err u204))

;; Reputation scoring weights (out of 100)
(define-constant PROPOSAL_SUCCESS_WEIGHT u30)
(define-constant MEMBER_ENGAGEMENT_WEIGHT u25)
(define-constant ACTIVITY_CONSISTENCY_WEIGHT u20)
(define-constant TREASURY_MANAGEMENT_WEIGHT u15)
(define-constant LONGEVITY_WEIGHT u10)

(define-constant MAX_REPUTATION_SCORE u1000)
(define-constant MIN_PROPOSALS_FOR_SCORING u3)

;; Helper function for minimum value
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Track fork reputation metrics
(define-map fork-reputation
  uint
  {
    total-proposals: uint,
    successful-proposals: uint,
    failed-proposals: uint,
    total-votes-cast: uint,
    active-members: uint,
    treasury-balance: uint,
    last-activity-block: uint,
    reputation-score: uint,
    score-updated-at: uint,
    activity-streak: uint
  }
)

;; Track member engagement per fork
(define-map member-engagement
  { fork-id: uint, member: principal }
  {
    proposals-created: uint,
    votes-cast: uint,
    treasury-contributions: uint,
    last-activity-block: uint,
    engagement-score: uint
  }
)

;; Historical reputation snapshots
(define-map reputation-history
  { fork-id: uint, snapshot-id: uint }
  {
    score: uint,
    block-height: uint,
    proposals-count: uint,
    member-count: uint,
    activity-level: uint
  }
)

;; Fork rankings and categories
(define-map reputation-rankings
  uint
  {
    rank: uint,
    category: (string-ascii 32),
    percentile: uint,
    last-ranked-at: uint
  }
)

;; Activity tracking for consistency scoring
(define-map weekly-activity
  { fork-id: uint, week-number: uint }
  {
    proposals-this-week: uint,
    votes-this-week: uint,
    new-members: uint,
    treasury-activity: uint
  }
)

(define-data-var snapshot-counter uint u0)
(define-data-var total-active-forks uint u0)

;; Initialize reputation tracking for a fork
(define-public (initialize-fork-reputation (fork-id uint))
  (let (
    (existing-reputation (map-get? fork-reputation fork-id))
  )
    (asserts! (is-none existing-reputation) ERR_REPUTATION_EXISTS)
    
    (map-set fork-reputation fork-id {
      total-proposals: u0,
      successful-proposals: u0,
      failed-proposals: u0,
      total-votes-cast: u0,
      active-members: u1,
      treasury-balance: u0,
      last-activity-block: stacks-block-height,
      reputation-score: u100, ;; Starting score
      score-updated-at: stacks-block-height,
      activity-streak: u0
    })
    
    (var-set total-active-forks (+ (var-get total-active-forks) u1))
    (ok true)
  )
)

;; Update reputation when a proposal is created
(define-public (record-proposal-created (fork-id uint) (creator principal))
  (let (
    (reputation (unwrap! (map-get? fork-reputation fork-id) ERR_FORK_NOT_FOUND))
    (member-engagement-data (default-to 
      { proposals-created: u0, votes-cast: u0, treasury-contributions: u0, last-activity-block: u0, engagement-score: u0 }
      (map-get? member-engagement { fork-id: fork-id, member: creator })))
    (current-week (/ stacks-block-height u1008)) ;; Approximate week in blocks
    (weekly-data (default-to 
      { proposals-this-week: u0, votes-this-week: u0, new-members: u0, treasury-activity: u0 }
      (map-get? weekly-activity { fork-id: fork-id, week-number: current-week })))
  )
    ;; Update fork reputation
    (map-set fork-reputation fork-id
      (merge reputation {
        total-proposals: (+ (get total-proposals reputation) u1),
        last-activity-block: stacks-block-height,
        activity-streak: (+ (get activity-streak reputation) u1)
      }))
    
    ;; Update member engagement
    (map-set member-engagement { fork-id: fork-id, member: creator }
      (merge member-engagement-data {
        proposals-created: (+ (get proposals-created member-engagement-data) u1),
        last-activity-block: stacks-block-height
      }))
    
    ;; Update weekly activity
    (map-set weekly-activity { fork-id: fork-id, week-number: current-week }
      (merge weekly-data {
        proposals-this-week: (+ (get proposals-this-week weekly-data) u1)
      }))
    
    (ok true)
  )
)

;; Update reputation when a proposal is executed
(define-public (record-proposal-executed (fork-id uint) (was-successful bool))
  (let (
    (reputation (unwrap! (map-get? fork-reputation fork-id) ERR_FORK_NOT_FOUND))
  )
    (if was-successful
      (map-set fork-reputation fork-id
        (merge reputation {
          successful-proposals: (+ (get successful-proposals reputation) u1),
          last-activity-block: stacks-block-height
        }))
      (map-set fork-reputation fork-id
        (merge reputation {
          failed-proposals: (+ (get failed-proposals reputation) u1),
          last-activity-block: stacks-block-height
        }))
    )
    (try! (calculate-reputation-score fork-id))
    (ok true)
  )
)

;; Record voting activity
(define-public (record-vote-cast (fork-id uint) (voter principal))
  (let (
    (reputation (unwrap! (map-get? fork-reputation fork-id) ERR_FORK_NOT_FOUND))
    (member-engagement-data (default-to 
      { proposals-created: u0, votes-cast: u0, treasury-contributions: u0, last-activity-block: u0, engagement-score: u0 }
      (map-get? member-engagement { fork-id: fork-id, member: voter })))
  )
    ;; Update fork reputation
    (map-set fork-reputation fork-id
      (merge reputation {
        total-votes-cast: (+ (get total-votes-cast reputation) u1),
        last-activity-block: stacks-block-height
      }))
    
    ;; Update member engagement
    (map-set member-engagement { fork-id: fork-id, member: voter }
      (merge member-engagement-data {
        votes-cast: (+ (get votes-cast member-engagement-data) u1),
        last-activity-block: stacks-block-height,
        engagement-score: (+ (get engagement-score member-engagement-data) u5)
      }))
    
    (ok true)
  )
)

;; Calculate comprehensive reputation score
(define-public (calculate-reputation-score (fork-id uint))
  (let (
    (reputation (unwrap! (map-get? fork-reputation fork-id) ERR_FORK_NOT_FOUND))
    (total-proposals (get total-proposals reputation))
    (successful-proposals (get successful-proposals reputation))
    (proposal-success-rate (if (> total-proposals u0) 
                            (/ (* successful-proposals u100) total-proposals) 
                            u0))
    (blocks-since-creation (- stacks-block-height (get score-updated-at reputation)))
    (activity-score (calculate-activity-score fork-id))
    (engagement-score (calculate-engagement-score fork-id))
    (treasury-score (calculate-treasury-score fork-id))
    (longevity-score (min-uint u100 (/ blocks-since-creation u144))) ;; Max score after ~1 day
  )
    ;; Only calculate reputation if fork has minimum activity
    (asserts! (>= total-proposals MIN_PROPOSALS_FOR_SCORING) ERR_INSUFFICIENT_ACTIVITY)
    
    (let (
      (weighted-score (+ 
        (/ (* proposal-success-rate PROPOSAL_SUCCESS_WEIGHT) u100)
        (/ (* engagement-score MEMBER_ENGAGEMENT_WEIGHT) u100)
        (/ (* activity-score ACTIVITY_CONSISTENCY_WEIGHT) u100)
        (/ (* treasury-score TREASURY_MANAGEMENT_WEIGHT) u100)
        (/ (* longevity-score LONGEVITY_WEIGHT) u100)))
      (final-score (min-uint MAX_REPUTATION_SCORE (* weighted-score u10))) ;; Scale to 0-1000
    )
      (map-set fork-reputation fork-id
        (merge reputation {
          reputation-score: final-score,
          score-updated-at: stacks-block-height
        }))
      
      (ok final-score)
    )
  )
)

;; Calculate activity consistency score
(define-private (calculate-activity-score (fork-id uint))
  (let (
    (reputation (unwrap-panic (map-get? fork-reputation fork-id)))
    (blocks-since-activity (- stacks-block-height (get last-activity-block reputation)))
    (activity-recency (if (< blocks-since-activity u1440) u100 ;; Active within 10 days
                        (if (< blocks-since-activity u4320) u70  ;; Active within 30 days
                          (if (< blocks-since-activity u8640) u40 ;; Active within 60 days
                            u10))))
    (streak-bonus (min-uint u30 (/ (get activity-streak reputation) u2)))
  )
    (+ activity-recency streak-bonus)
  )
)

;; Calculate member engagement score
(define-private (calculate-engagement-score (fork-id uint))
  (let (
    (reputation (unwrap-panic (map-get? fork-reputation fork-id)))
    (total-votes (get total-votes-cast reputation))
    (active-members (get active-members reputation))
    (engagement-ratio (if (> active-members u0) 
                        (/ total-votes active-members) 
                        u0))
  )
    (min-uint u100 (* engagement-ratio u10))
  )
)

;; Calculate treasury management score
(define-private (calculate-treasury-score (fork-id uint))
  (let (
    (reputation (unwrap-panic (map-get? fork-reputation fork-id)))
    (treasury-balance (get treasury-balance reputation))
  )
    (if (> treasury-balance u0)
      (min-uint u100 (/ treasury-balance u10000000)) ;; Score based on treasury size
      u20) ;; Minimum score for having a treasury system
  )
)

;; Create reputation snapshot
(define-public (create-reputation-snapshot (fork-id uint))
  (let (
    (reputation (unwrap! (map-get? fork-reputation fork-id) ERR_FORK_NOT_FOUND))
    (snapshot-id (+ (var-get snapshot-counter) u1))
  )
    (var-set snapshot-counter snapshot-id)
    
    (map-set reputation-history { fork-id: fork-id, snapshot-id: snapshot-id } {
      score: (get reputation-score reputation),
      block-height: stacks-block-height,
      proposals-count: (get total-proposals reputation),
      member-count: (get active-members reputation),
      activity-level: (get activity-streak reputation)
    })
    
    (ok snapshot-id)
  )
)

;; Get fork reputation data
(define-read-only (get-fork-reputation (fork-id uint))
  (map-get? fork-reputation fork-id)
)

;; Get member engagement data
(define-read-only (get-member-engagement (fork-id uint) (member principal))
  (map-get? member-engagement { fork-id: fork-id, member: member })
)

;; Get reputation history
(define-read-only (get-reputation-history (fork-id uint) (snapshot-id uint))
  (map-get? reputation-history { fork-id: fork-id, snapshot-id: snapshot-id })
)

;; Get fork ranking
(define-read-only (get-fork-ranking (fork-id uint))
  (map-get? reputation-rankings fork-id)
)

;; Calculate reputation percentile
(define-read-only (calculate-reputation-percentile (fork-id uint))
  (match (map-get? fork-reputation fork-id)
    reputation
    (let (
      (fork-score (get reputation-score reputation))
      (total-forks (var-get total-active-forks))
    )
      (some {
        score: fork-score,
        category: (if (>= fork-score u800) "Excellent"
                    (if (>= fork-score u600) "Good"  
                      (if (>= fork-score u400) "Average"
                        (if (>= fork-score u200) "Poor"
                          "Inactive")))),
        percentile: (min-uint u100 (/ (* fork-score u100) MAX_REPUTATION_SCORE)),
        total-forks: total-forks
      })
    )
    none
  )
)

;; Get system reputation stats
(define-read-only (get-system-reputation-stats)
  {
    total-active-forks: (var-get total-active-forks),
    total-snapshots: (var-get snapshot-counter),
    scoring-weights: {
      proposal-success: PROPOSAL_SUCCESS_WEIGHT,
      member-engagement: MEMBER_ENGAGEMENT_WEIGHT,
      activity-consistency: ACTIVITY_CONSISTENCY_WEIGHT,
      treasury-management: TREASURY_MANAGEMENT_WEIGHT,
      longevity: LONGEVITY_WEIGHT
    },
    max-score: MAX_REPUTATION_SCORE
  }
)
