(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NGO_NOT_FOUND (err u101))
(define-constant ERR_DONATION_NOT_FOUND (err u102))
(define-constant ERR_EXPENSE_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_NGO_ALREADY_EXISTS (err u106))

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