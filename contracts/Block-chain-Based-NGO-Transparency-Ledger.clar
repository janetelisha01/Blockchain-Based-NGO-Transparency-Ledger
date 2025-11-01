(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NGO_NOT_FOUND (err u101))
(define-constant ERR_DONATION_NOT_FOUND (err u102))
(define-constant ERR_EXPENSE_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_NGO_ALREADY_EXISTS (err u106))

(define-constant ERR_ALERT_NOT_FOUND (err u107))
(define-constant ERR_ALREADY_REPORTED (err u108))
(define-constant ERR_INVALID_THRESHOLD (err u109))

(define-constant ERR_ESCROW_NOT_FOUND (err u111))
(define-constant ERR_ESCROW_LOCKED (err u112))
(define-constant ERR_CONDITION_NOT_MET (err u113))
(define-constant ERR_DEADLINE_PASSED (err u114))

(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u115))
(define-constant ERR_SUBSCRIPTION_NOT_ACTIVE (err u116))
(define-constant ERR_INTERVAL_NOT_READY (err u117))
(define-constant ERR_SUBSCRIPTION_EXHAUSTED (err u118))

(define-data-var next-subscription-id uint u1)

(define-data-var next-escrow-id uint u1)

(define-data-var next-milestone-id uint u1)

(define-map ngos
  { ngo-id: uint }
  {
    name: (string-ascii 100),
    wallet: principal,
    total-received: uint,
    total-spent: uint,
    is-verified: bool,
    registration-block: uint
  }
)

(define-map donations
  { donation-id: uint }
  {
    ngo-id: uint,
    donor: principal,
    amount: uint,
    purpose: (string-ascii 200),
    timestamp: uint,
    stacks-block-height: uint
  }
)

(define-map expenses
  { expense-id: uint }
  {
    ngo-id: uint,
    amount: uint,
    category: (string-ascii 50),
    description: (string-ascii 300),
    recipient: (string-ascii 100),
    timestamp: uint,
    approved-by: principal,
    stacks-block-height: uint
  }
)

(define-data-var next-ngo-id uint u1)
(define-data-var next-donation-id uint u1)
(define-data-var next-expense-id uint u1)

(define-public (register-ngo (name (string-ascii 100)) (wallet principal))
  (let
    (
      (ngo-id (var-get next-ngo-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? ngos { ngo-id: ngo-id })) ERR_NGO_ALREADY_EXISTS)
    (map-set ngos
      { ngo-id: ngo-id }
      {
        name: name,
        wallet: wallet,
        total-received: u0,
        total-spent: u0,
        is-verified: true,
        registration-block: stacks-block-height
      }
    )
    (var-set next-ngo-id (+ ngo-id u1))
    (ok ngo-id)
  )
)

(define-public (make-donation (ngo-id uint) (amount uint) (purpose (string-ascii 200)))
  (let
    (
      (donation-id (var-get next-donation-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (get wallet ngo-data)))
    (map-set donations
      { donation-id: donation-id }
      {
        ngo-id: ngo-id,
        donor: tx-sender,
        amount: amount,
        purpose: purpose,
        timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height)),
        stacks-block-height: stacks-block-height
      }
    )
    (map-set ngos
      { ngo-id: ngo-id }
      (merge ngo-data { total-received: (+ (get total-received ngo-data) amount) })
    )
    (var-set next-donation-id (+ donation-id u1))
    (ok donation-id)
  )
)

(define-public (record-expense 
  (ngo-id uint) 
  (amount uint) 
  (category (string-ascii 50)) 
  (description (string-ascii 300)) 
  (recipient (string-ascii 100))
)
  (let
    (
      (expense-id (var-get next-expense-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get wallet ngo-data)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get total-received ngo-data) (+ (get total-spent ngo-data) amount)) ERR_INSUFFICIENT_FUNDS)
    (map-set expenses
      { expense-id: expense-id }
      {
        ngo-id: ngo-id,
        amount: amount,
        category: category,
        description: description,
        recipient: recipient,
        timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height)),
        approved-by: tx-sender,
        stacks-block-height: stacks-block-height
      }
    )
    (map-set ngos
      { ngo-id: ngo-id }
      (merge ngo-data { total-spent: (+ (get total-spent ngo-data) amount) })
    )
    (var-set next-expense-id (+ expense-id u1))
    (ok expense-id)
  )
)

(define-read-only (get-ngo (ngo-id uint))
  (map-get? ngos { ngo-id: ngo-id })
)

(define-read-only (get-donation (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

(define-read-only (get-expense (expense-id uint))
  (map-get? expenses { expense-id: expense-id })
)

(define-read-only (get-ngo-balance (ngo-id uint))
  (match (map-get? ngos { ngo-id: ngo-id })
    ngo-data (ok (- (get total-received ngo-data) (get total-spent ngo-data)))
    ERR_NGO_NOT_FOUND
  )
)

(define-read-only (get-ngo-transparency-score (ngo-id uint))
  (match (map-get? ngos { ngo-id: ngo-id })
    ngo-data 
    (let
      (
        (total-received (get total-received ngo-data))
        (total-spent (get total-spent ngo-data))
      )
      (if (is-eq total-received u0)
        (ok u100)
        (ok (/ (* (- total-received total-spent) u100) total-received))
      )
    )
    ERR_NGO_NOT_FOUND
  )
)

(define-read-only (get-total-ngos)
  (- (var-get next-ngo-id) u1)
)

(define-read-only (get-total-donations)
  (- (var-get next-donation-id) u1)
)

(define-read-only (get-total-expenses)
  (- (var-get next-expense-id) u1)
)

(define-public (verify-ngo (ngo-id uint))
  (let
    (
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set ngos
      { ngo-id: ngo-id }
      (merge ngo-data { is-verified: true })
    )
    (ok true)
  )
)

(define-public (unverify-ngo (ngo-id uint))
  (let
    (
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set ngos
      { ngo-id: ngo-id }
      (merge ngo-data { is-verified: false })
    )
    (ok true)
  )
)

(define-read-only (is-ngo-verified (ngo-id uint))
  (match (map-get? ngos { ngo-id: ngo-id })
    ngo-data (ok (get is-verified ngo-data))
    ERR_NGO_NOT_FOUND
  )
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)

(define-map ngo-milestones
  { milestone-id: uint }
  {
    ngo-id: uint,
    title: (string-ascii 100),
    target-amount: uint,
    current-progress: uint,
    deadline-block: uint,
    is-completed: bool,
    completion-block: (optional uint),
    category: (string-ascii 50)
  }
)

(define-map ngo-performance-metrics
  { ngo-id: uint }
  {
    total-milestones: uint,
    completed-milestones: uint,
    avg-completion-time: uint,
    efficiency-score: uint,
    last-updated: uint
  }
)

(define-public (create-milestone 
  (ngo-id uint) 
  (title (string-ascii 100)) 
  (target-amount uint) 
  (deadline-blocks uint)
  (category (string-ascii 50))
)
  (let
    (
      (milestone-id (var-get next-milestone-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
      (current-metrics (default-to 
        { total-milestones: u0, completed-milestones: u0, avg-completion-time: u0, efficiency-score: u0, last-updated: u0 }
        (map-get? ngo-performance-metrics { ngo-id: ngo-id })
      ))
    )
    (asserts! (is-eq tx-sender (get wallet ngo-data)) ERR_UNAUTHORIZED)
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (map-set ngo-milestones
      { milestone-id: milestone-id }
      {
        ngo-id: ngo-id,
        title: title,
        target-amount: target-amount,
        current-progress: u0,
        deadline-block: (+ stacks-block-height deadline-blocks),
        is-completed: false,
        completion-block: none,
        category: category
      }
    )
    (map-set ngo-performance-metrics
      { ngo-id: ngo-id }
      (merge current-metrics { 
        total-milestones: (+ (get total-milestones current-metrics) u1),
        last-updated: stacks-block-height
      })
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (update-milestone-progress (milestone-id uint) (progress-amount uint))
  (let
    (
      (milestone-data (unwrap! (map-get? ngo-milestones { milestone-id: milestone-id }) ERR_EXPENSE_NOT_FOUND))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: (get ngo-id milestone-data) }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get wallet ngo-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-completed milestone-data)) ERR_INVALID_AMOUNT)
    (let
      (
        (new-progress (+ (get current-progress milestone-data) progress-amount))
        (is-now-completed (>= new-progress (get target-amount milestone-data)))
      )
      (map-set ngo-milestones
        { milestone-id: milestone-id }
        (merge milestone-data {
          current-progress: new-progress,
          is-completed: is-now-completed,
          completion-block: (if is-now-completed (some stacks-block-height) none)
        })
      )
      (if is-now-completed
        (begin
          (update-performance-metrics (get ngo-id milestone-data))
          (ok true)
        )
        (ok true)
      )
    )
  )
)

(define-private (update-performance-metrics (ngo-id uint))
  (let
    (
      (current-metrics (default-to 
        { total-milestones: u0, completed-milestones: u0, avg-completion-time: u0, efficiency-score: u0, last-updated: u0 }
        (map-get? ngo-performance-metrics { ngo-id: ngo-id })
      ))
      (new-completed (+ (get completed-milestones current-metrics) u1))
      (efficiency (if (> (get total-milestones current-metrics) u0)
        (/ (* new-completed u100) (get total-milestones current-metrics))
        u0
      ))
    )
    (map-set ngo-performance-metrics
      { ngo-id: ngo-id }
      (merge current-metrics {
        completed-milestones: new-completed,
        efficiency-score: efficiency,
        last-updated: stacks-block-height
      })
    )
  )
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? ngo-milestones { milestone-id: milestone-id })
)

(define-read-only (get-ngo-performance (ngo-id uint))
  (map-get? ngo-performance-metrics { ngo-id: ngo-id })
)

(define-read-only (get-milestone-progress-percentage (milestone-id uint))
  (match (map-get? ngo-milestones { milestone-id: milestone-id })
    milestone-data 
    (ok (/ (* (get current-progress milestone-data) u100) (get target-amount milestone-data)))
    ERR_EXPENSE_NOT_FOUND
  )
)


(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-data-var next-alert-id uint u1)
(define-data-var fraud-threshold-percentage uint u20)

(define-map fraud-alerts
  { alert-id: uint }
  {
    ngo-id: uint,
    alert-type: (string-ascii 20),
    severity: uint,
    description: (string-ascii 200),
    triggered-by: principal,
    timestamp: uint,
    is-resolved: bool,
    resolution-notes: (optional (string-ascii 300))
  }
)

(define-map community-reports
  { ngo-id: uint, reporter: principal }
  {
    report-count: uint,
    last-report-block: uint,
    total-severity: uint
  }
)

(define-map ngo-alert-stats
  { ngo-id: uint }
  {
    total-alerts: uint,
    active-alerts: uint,
    community-reports: uint,
    risk-score: uint
  }
)

(define-public (trigger-spending-alert (ngo-id uint) (expense-amount uint))
  (let
    (
      (alert-id (var-get next-alert-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
      (spending-percentage (if (> (get total-received ngo-data) u0)
        (/ (* expense-amount u100) (get total-received ngo-data))
        u0))
      (threshold (var-get fraud-threshold-percentage))
    )
    (if (>= spending-percentage threshold)
      (begin
        (map-set fraud-alerts
          { alert-id: alert-id }
          {
            ngo-id: ngo-id,
            alert-type: "HIGH_SPENDING",
            severity: (min u10 (/ spending-percentage u10)),
            description: "Expense exceeds risk threshold",
            triggered-by: tx-sender,
            timestamp: stacks-block-height,
            is-resolved: false,
            resolution-notes: none
          }
        )
        (update-alert-stats ngo-id true)
        (var-set next-alert-id (+ alert-id u1))
        (ok alert-id)
      )
      (ok u0)
    )
  )
)

(define-public (submit-community-report (ngo-id uint) (severity uint) (reason (string-ascii 200)))
  (let
    (
      (alert-id (var-get next-alert-id))
      (existing-report (map-get? community-reports { ngo-id: ngo-id, reporter: tx-sender }))
    )
    (asserts! (is-none existing-report) ERR_ALREADY_REPORTED)
    (asserts! (<= severity u10) ERR_INVALID_THRESHOLD)
    (map-set fraud-alerts
      { alert-id: alert-id }
      {
        ngo-id: ngo-id,
        alert-type: "COMMUNITY",
        severity: severity,
        description: reason,
        triggered-by: tx-sender,
        timestamp: stacks-block-height,
        is-resolved: false,
        resolution-notes: none
      }
    )
    (map-set community-reports
      { ngo-id: ngo-id, reporter: tx-sender }
      {
        report-count: u1,
        last-report-block: stacks-block-height,
        total-severity: severity
      }
    )
    (update-alert-stats ngo-id true)
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

(define-private (update-alert-stats (ngo-id uint) (is-new-alert bool))
  (let
    (
      (current-stats (default-to 
        { total-alerts: u0, active-alerts: u0, community-reports: u0, risk-score: u0 }
        (map-get? ngo-alert-stats { ngo-id: ngo-id })
      ))
    )
    (map-set ngo-alert-stats
      { ngo-id: ngo-id }
      (merge current-stats {
        total-alerts: (if is-new-alert (+ (get total-alerts current-stats) u1) (get total-alerts current-stats)),
        active-alerts: (if is-new-alert (+ (get active-alerts current-stats) u1) (- (get active-alerts current-stats) u1)),
        risk-score: (min u100 (* (get active-alerts current-stats) u15))
      })
    )
  )
)

(define-read-only (get-fraud-alert (alert-id uint))
  (map-get? fraud-alerts { alert-id: alert-id })
)

(define-read-only (get-ngo-risk-score (ngo-id uint))
  (match (map-get? ngo-alert-stats { ngo-id: ngo-id })
    stats (ok (get risk-score stats))
    (ok u0)
  )
)

(define-constant ERR_DONOR_NOT_FOUND (err u110))

(define-map donor-reputation
  { donor: principal }
  {
    total-donated: uint,
    donation-count: uint,
    reputation-score: uint,
    achievement-tier: uint,
    streak-count: uint,
    last-donation-block: uint,
    badges: uint
  }
)

(define-map donor-achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    requirement-type: (string-ascii 20),
    threshold-value: uint,
    badge-points: uint
  }
)

(define-data-var next-achievement-id uint u1)

(define-private (initialize-achievements)
  (begin
    (map-set donor-achievements { achievement-id: u1 } 
      { name: "First Drop", description: "Made your first donation", 
        requirement-type: "DONATION_COUNT", threshold-value: u1, badge-points: u10 })
    (map-set donor-achievements { achievement-id: u2 } 
      { name: "Generous Soul", description: "Donated over 10,000 STX total", 
        requirement-type: "TOTAL_AMOUNT", threshold-value: u10000000000, badge-points: u50 })
    (map-set donor-achievements { achievement-id: u3 } 
      { name: "Consistent Giver", description: "Made 10+ donations", 
        requirement-type: "DONATION_COUNT", threshold-value: u10, badge-points: u25 })
    (var-set next-achievement-id u4)
  )
)

(define-public (update-donor-reputation (donor principal) (donation-amount uint))
  (let
    (
      (current-reputation (default-to 
        { total-donated: u0, donation-count: u0, reputation-score: u0, 
          achievement-tier: u0, streak-count: u0, last-donation-block: u0, badges: u0 }
        (map-get? donor-reputation { donor: donor })
      ))
      (new-total (+ (get total-donated current-reputation) donation-amount))
      (new-count (+ (get donation-count current-reputation) u1))
      (blocks-since-last (- stacks-block-height (get last-donation-block current-reputation)))
      (streak-bonus (if (< blocks-since-last u1440) u1 u0))
      (new-streak (+ (get streak-count current-reputation) streak-bonus))
      (base-score (+ (* new-count u10) (/ new-total u1000000)))
      (streak-multiplier (+ u100 (* new-streak u5)))
      (final-score (/ (* base-score streak-multiplier) u100))
      (new-tier (min u5 (/ final-score u100)))
    )
    (map-set donor-reputation
      { donor: donor }
      {
        total-donated: new-total,
        donation-count: new-count,
        reputation-score: final-score,
        achievement-tier: new-tier,
        streak-count: new-streak,
        last-donation-block: stacks-block-height,
        badges: (check-and-award-badges donor new-total new-count)
      }
    )
    (ok true)
  )
)

(define-private (check-and-award-badges (donor principal) (total-donated uint) (donation-count uint))
  (let
    (
      (current-badges (get badges (default-to 
        { total-donated: u0, donation-count: u0, reputation-score: u0, 
          achievement-tier: u0, streak-count: u0, last-donation-block: u0, badges: u0 }
        (map-get? donor-reputation { donor: donor })
      )))
      (badge-1 (if (>= donation-count u1) u1 u0))
      (badge-2 (if (>= total-donated u10000000000) u2 u0))
      (badge-3 (if (>= donation-count u10) u4 u0))
    )
    (+ current-badges badge-1 badge-2 badge-3)
  )
)

(define-read-only (get-donor-reputation (donor principal))
  (map-get? donor-reputation { donor: donor })
)

(define-read-only (get-donor-tier-name (tier uint))
  (if (is-eq tier u0) "Newcomer"
  (if (is-eq tier u1) "Supporter" 
  (if (is-eq tier u2) "Advocate"
  (if (is-eq tier u3) "Champion"
  (if (is-eq tier u4) "Guardian" "Legend")))))
)


(define-map escrow-deposits
  { escrow-id: uint }
  {
    donor: principal,
    ngo-id: uint,
    amount: uint,
    condition-type: (string-ascii 20),
    condition-value: uint,
    deadline-block: uint,
    is-released: bool,
    release-block: (optional uint),
    created-at: uint
  }
)

(define-map escrow-balances
  { ngo-id: uint }
  { total-locked: uint, total-released: uint, pending-count: uint }
)

(define-public (deposit-to-escrow 
  (ngo-id uint) 
  (amount uint) 
  (condition-type (string-ascii 20))
  (condition-value uint)
  (deadline-blocks uint)
)
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
      (current-balance (default-to 
        { total-locked: u0, total-released: u0, pending-count: u0 }
        (map-get? escrow-balances { ngo-id: ngo-id })
      ))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrow-deposits
      { escrow-id: escrow-id }
      {
        donor: tx-sender,
        ngo-id: ngo-id,
        amount: amount,
        condition-type: condition-type,
        condition-value: condition-value,
        deadline-block: (+ stacks-block-height deadline-blocks),
        is-released: false,
        release-block: none,
        created-at: stacks-block-height
      }
    )
    (map-set escrow-balances
      { ngo-id: ngo-id }
      (merge current-balance {
        total-locked: (+ (get total-locked current-balance) amount),
        pending-count: (+ (get pending-count current-balance) u1)
      })
    )
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (release-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-deposits { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: (get ngo-id escrow-data) }) ERR_NGO_NOT_FOUND))
      (escrow-balance (unwrap! (map-get? escrow-balances { ngo-id: (get ngo-id escrow-data) }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (not (get is-released escrow-data)) ERR_ESCROW_LOCKED)
    (asserts! (<= stacks-block-height (get deadline-block escrow-data)) ERR_DEADLINE_PASSED)
    (asserts! (check-condition escrow-data) ERR_CONDITION_NOT_MET)
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get wallet ngo-data))))
    (map-set escrow-deposits
      { escrow-id: escrow-id }
      (merge escrow-data { is-released: true, release-block: (some stacks-block-height) })
    )
    (map-set escrow-balances
      { ngo-id: (get ngo-id escrow-data) }
      (merge escrow-balance {
        total-released: (+ (get total-released escrow-balance) (get amount escrow-data)),
        pending-count: (- (get pending-count escrow-balance) u1)
      })
    )
    (ok true)
  )
)

(define-private (check-condition (escrow-data {
  donor: principal, ngo-id: uint, amount: uint, condition-type: (string-ascii 20),
  condition-value: uint, deadline-block: uint, is-released: bool,
  release-block: (optional uint), created-at: uint
}))
  (if (is-eq (get condition-type escrow-data) "MILESTONE")
    (match (map-get? ngo-milestones { milestone-id: (get condition-value escrow-data) })
      milestone (get is-completed milestone)
      false
    )
    true
  )
)

(define-public (refund-escrow (escrow-id uint))
  (let
    (
      (escrow-data (unwrap! (map-get? escrow-deposits { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (escrow-balance (unwrap! (map-get? escrow-balances { ngo-id: (get ngo-id escrow-data) }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (not (get is-released escrow-data)) ERR_ESCROW_LOCKED)
    (asserts! (> stacks-block-height (get deadline-block escrow-data)) ERR_CONDITION_NOT_MET)
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get donor escrow-data))))
    (map-set escrow-deposits
      { escrow-id: escrow-id }
      (merge escrow-data { is-released: true })
    )
    (map-set escrow-balances
      { ngo-id: (get ngo-id escrow-data) }
      (merge escrow-balance { pending-count: (- (get pending-count escrow-balance) u1) })
    )
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-deposits { escrow-id: escrow-id })
)

(define-read-only (get-ngo-escrow-balance (ngo-id uint))
  (map-get? escrow-balances { ngo-id: ngo-id })
)


(define-map donor-subscriptions
  { subscription-id: uint }
  {
    donor: principal,
    ngo-id: uint,
    amount-per-cycle: uint,
    interval-blocks: uint,
    total-cycles: uint,
    completed-cycles: uint,
    next-execution-block: uint,
    is-active: bool,
    created-at: uint,
    last-execution: (optional uint)
  }
)

(define-map subscription-stats
  { ngo-id: uint }
  { active-subscriptions: uint, total-recurring-value: uint, cycles-completed: uint }
)

(define-public (create-subscription
  (ngo-id uint)
  (amount-per-cycle uint)
  (interval-blocks uint)
  (total-cycles uint)
)
  (let
    (
      (subscription-id (var-get next-subscription-id))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: ngo-id }) ERR_NGO_NOT_FOUND))
      (current-stats (default-to
        { active-subscriptions: u0, total-recurring-value: u0, cycles-completed: u0 }
        (map-get? subscription-stats { ngo-id: ngo-id })
      ))
    )
    (asserts! (> amount-per-cycle u0) ERR_INVALID_AMOUNT)
    (asserts! (> total-cycles u0) ERR_INVALID_AMOUNT)
    (map-set donor-subscriptions
      { subscription-id: subscription-id }
      {
        donor: tx-sender,
        ngo-id: ngo-id,
        amount-per-cycle: amount-per-cycle,
        interval-blocks: interval-blocks,
        total-cycles: total-cycles,
        completed-cycles: u0,
        next-execution-block: (+ stacks-block-height interval-blocks),
        is-active: true,
        created-at: stacks-block-height,
        last-execution: none
      }
    )
    (map-set subscription-stats
      { ngo-id: ngo-id }
      (merge current-stats {
        active-subscriptions: (+ (get active-subscriptions current-stats) u1),
        total-recurring-value: (+ (get total-recurring-value current-stats) (* amount-per-cycle total-cycles))
      })
    )
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (execute-subscription (subscription-id uint))
  (let
    (
      (sub-data (unwrap! (map-get? donor-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (ngo-data (unwrap! (map-get? ngos { ngo-id: (get ngo-id sub-data) }) ERR_NGO_NOT_FOUND))
    )
    (asserts! (get is-active sub-data) ERR_SUBSCRIPTION_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get next-execution-block sub-data)) ERR_INTERVAL_NOT_READY)
    (asserts! (< (get completed-cycles sub-data) (get total-cycles sub-data)) ERR_SUBSCRIPTION_EXHAUSTED)
    (try! (stx-transfer? (get amount-per-cycle sub-data) (get donor sub-data) (get wallet ngo-data)))
    (let
      (
        (new-completed (+ (get completed-cycles sub-data) u1))
        (is-finished (>= new-completed (get total-cycles sub-data)))
      )
      (map-set donor-subscriptions
        { subscription-id: subscription-id }
        (merge sub-data {
          completed-cycles: new-completed,
          next-execution-block: (+ stacks-block-height (get interval-blocks sub-data)),
          is-active: (not is-finished),
          last-execution: (some stacks-block-height)
        })
      )
      (unwrap! (update-donor-reputation (get donor sub-data) (get amount-per-cycle sub-data)) (err u0))
      (ok true)
    )
  )
)

(define-public (pause-subscription (subscription-id uint))
  (let
    ((sub-data (unwrap! (map-get? donor-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get donor sub-data)) ERR_UNAUTHORIZED)
    (map-set donor-subscriptions { subscription-id: subscription-id } (merge sub-data { is-active: false }))
    (ok true)
  )
)

(define-public (resume-subscription (subscription-id uint))
  (let
    ((sub-data (unwrap! (map-get? donor-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get donor sub-data)) ERR_UNAUTHORIZED)
    (asserts! (< (get completed-cycles sub-data) (get total-cycles sub-data)) ERR_SUBSCRIPTION_EXHAUSTED)
    (map-set donor-subscriptions { subscription-id: subscription-id } (merge sub-data { is-active: true }))
    (ok true)
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? donor-subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-ngo-subscription-stats (ngo-id uint))
  (map-get? subscription-stats { ngo-id: ngo-id })
)

(define-read-only (is-subscription-executable (subscription-id uint))
  (match (map-get? donor-subscriptions { subscription-id: subscription-id })
    sub-data (ok (and 
      (get is-active sub-data)
      (>= stacks-block-height (get next-execution-block sub-data))
      (< (get completed-cycles sub-data) (get total-cycles sub-data))
    ))
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)