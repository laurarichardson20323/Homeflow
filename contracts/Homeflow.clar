(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_ESCROW_NOT_PENDING (err u105))
(define-constant ERR_NOT_BUYER_OR_SELLER (err u106))
(define-constant ERR_VERIFICATION_FAILED (err u107))
(define-constant ERR_ESCROW_EXPIRED (err u108))
(define-constant ERR_MORTGAGE_NOT_FOUND (err u109))
(define-constant ERR_PAYMENT_ALREADY_MADE (err u110))
(define-constant ERR_PAYMENT_OVERDUE (err u111))
(define-constant ERR_INVALID_PAYMENT_AMOUNT (err u112))
(define-constant ERR_MORTGAGE_COMPLETED (err u113))
(define-constant ERR_MORTGAGE_DEFAULTED (err u114))
(define-constant ERR_PAYMENT_NOT_DUE (err u115))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u116))
(define-constant ERR_INSPECTION_NOT_FOUND (err u117))
(define-constant ERR_INSPECTOR_NOT_AUTHORIZED (err u118))
(define-constant ERR_INSPECTION_ALREADY_EXISTS (err u119))
(define-constant ERR_FINDING_NOT_FOUND (err u120))
(define-constant ERR_INVALID_SEVERITY (err u121))
(define-constant ERR_REPAIR_ALREADY_COMPLETED (err u122))
(define-constant ERR_INSPECTION_COMPLETED (err u123))
(define-constant ERR_FINDINGS_PENDING (err u124))

(define-data-var escrow-counter uint u0)
(define-data-var mortgage-counter uint u0)
(define-data-var inspection-counter uint u0)
(define-data-var finding-counter uint u0)

(define-map escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    property-id: (string-ascii 64),
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint,
    verification-required: bool,
    verified: bool
  }
)

(define-map verifiers
  { verifier: principal }
  { authorized: bool }
)

(define-map escrow-funds
  { escrow-id: uint }
  { locked-amount: uint }
)

(define-map mortgages
  { mortgage-id: uint }
  {
    borrower: principal,
    lender: principal,
    total-amount: uint,
    monthly-payment: uint,
    total-payments: uint,
    payments-made: uint,
    property-id: (string-ascii 64),
    interest-rate: uint,
    start-date: uint,
    payment-interval: uint,
    status: (string-ascii 20),
    last-payment-date: uint,
    next-payment-due: uint,
    default-threshold: uint
  }
)

(define-map mortgage-payments
  { mortgage-id: uint, payment-number: uint }
  {
    amount: uint,
    payment-date: uint,
    principal-portion: uint,
    interest-portion: uint,
    remaining-balance: uint,
    late-fee: uint,
    status: (string-ascii 20)
  }
)

(define-map mortgage-balances
  { mortgage-id: uint }
  { 
    remaining-principal: uint,
    total-interest-paid: uint,
    total-late-fees: uint,
    escrow-balance: uint
  }
)

(define-map inspectors
  { inspector: principal }
  {
    authorized: bool,
    license-number: (string-ascii 32),
    specialty: (string-ascii 32),
    rating: uint,
    total-inspections: uint
  }
)

(define-map property-inspections
  { inspection-id: uint }
  {
    property-id: (string-ascii 64),
    escrow-id: uint,
    inspector: principal,
    inspection-type: (string-ascii 32),
    scheduled-date: uint,
    completed-date: uint,
    status: (string-ascii 20),
    overall-rating: uint,
    total-findings: uint,
    critical-findings: uint,
    estimated-repair-cost: uint
  }
)

(define-map inspection-findings
  { finding-id: uint }
  {
    inspection-id: uint,
    area: (string-ascii 32),
    finding-type: (string-ascii 32),
    severity: uint,
    description: (string-ascii 128),
    estimated-cost: uint,
    photo-hash: (string-ascii 64),
    repair-required: bool,
    repair-completed: bool,
    repair-verified-by: (optional principal),
    repair-completion-date: uint
  }
)

(define-map inspection-reports
  { inspection-id: uint }
  {
    summary: (string-ascii 256),
    recommendations: (string-ascii 256),
    pass-fail-status: (string-ascii 20),
    inspector-notes: (string-ascii 128),
    buyer-acknowledged: bool,
    seller-acknowledged: bool
  }
)

(define-public (create-escrow (seller principal) (amount uint) (property-id (string-ascii 64)) (duration uint))
  (let
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (expires-at (+ stacks-block-height duration))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? escrows { escrow-id: escrow-id })) ERR_ESCROW_ALREADY_EXISTS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      {
        buyer: tx-sender,
        seller: seller,
        amount: amount,
        property-id: property-id,
        status: "pending",
        created-at: stacks-block-height,
        expires-at: expires-at,
        verification-required: true,
        verified: false
      }
    )
    
    (map-set escrow-funds
      { escrow-id: escrow-id }
      { locked-amount: amount }
    )
    
    (var-set escrow-counter escrow-id)
    (ok escrow-id)
  )
)

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set verifiers { verifier: verifier } { authorized: true })
    (ok true)
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set verifiers { verifier: verifier } { authorized: false })
    (ok true)
  )
)

(define-public (verify-property (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (verifier-status (default-to { authorized: false } (map-get? verifiers { verifier: tx-sender })))
    )
    (asserts! (get authorized verifier-status) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "pending") ERR_ESCROW_NOT_PENDING)
    (asserts! (< stacks-block-height (get expires-at escrow)) ERR_ESCROW_EXPIRED)
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { verified: true })
    )
    (ok true)
  )
)

(define-public (complete-sale (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (funds (unwrap! (map-get? escrow-funds { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_BUYER_OR_SELLER)
    (asserts! (is-eq (get status escrow) "pending") ERR_ESCROW_NOT_PENDING)
    (asserts! (get verified escrow) ERR_VERIFICATION_FAILED)
    (asserts! (< stacks-block-height (get expires-at escrow)) ERR_ESCROW_EXPIRED)
    
    (try! (as-contract (stx-transfer? (get locked-amount funds) tx-sender (get seller escrow))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "completed" })
    )
    
    (map-delete escrow-funds { escrow-id: escrow-id })
    (ok true)
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (funds (unwrap! (map-get? escrow-funds { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow))
      (is-eq tx-sender (get seller escrow))
      (>= stacks-block-height (get expires-at escrow))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "pending") ERR_ESCROW_NOT_PENDING)
    
    (try! (as-contract (stx-transfer? (get locked-amount funds) tx-sender (get buyer escrow))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "cancelled" })
    )
    
    (map-delete escrow-funds { escrow-id: escrow-id })
    (ok true)
  )
)

(define-public (dispute-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_BUYER_OR_SELLER)
    (asserts! (is-eq (get status escrow) "pending") ERR_ESCROW_NOT_PENDING)
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "disputed" })
    )
    (ok true)
  )
)

(define-public (resolve-dispute (escrow-id uint) (release-to-seller bool))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (funds (unwrap! (map-get? escrow-funds { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (recipient (if release-to-seller (get seller escrow) (get buyer escrow)))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "disputed") ERR_ESCROW_NOT_PENDING)
    
    (try! (as-contract (stx-transfer? (get locked-amount funds) tx-sender recipient)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: (if release-to-seller "completed" "cancelled") })
    )
    
    (map-delete escrow-funds { escrow-id: escrow-id })
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-escrow-funds (escrow-id uint))
  (map-get? escrow-funds { escrow-id: escrow-id })
)

;; (define-read-only (is-verifier (verifier principal))
;;   (default-to false (get authorized (default-to { authorized: false } (map-get? verifiers { verifier: verifier }))))
;; )

(define-read-only (get-escrow-count)
  (var-get escrow-counter)
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow (>= stacks-block-height (get expires-at escrow))
    false
  )
)

(define-read-only (get-escrow-status (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow (get status escrow)
    "not-found"
  )
)

(define-public (create-mortgage (lender principal) (total-amount uint) (monthly-payment uint) (total-payments uint) (property-id (string-ascii 64)) (interest-rate uint) (payment-interval uint))
  (let
    (
      (mortgage-id (+ (var-get mortgage-counter) u1))
      (start-date stacks-block-height)
      (next-payment-due (+ stacks-block-height payment-interval))
      (default-threshold u30)
    )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> monthly-payment u0) ERR_INVALID_PAYMENT_AMOUNT)
    (asserts! (> total-payments u0) ERR_INVALID_PAYMENT_AMOUNT)
    (asserts! (> payment-interval u0) ERR_INVALID_PAYMENT_AMOUNT)
    (asserts! (is-none (map-get? mortgages { mortgage-id: mortgage-id })) ERR_MORTGAGE_NOT_FOUND)
    
    (map-set mortgages
      { mortgage-id: mortgage-id }
      {
        borrower: tx-sender,
        lender: lender,
        total-amount: total-amount,
        monthly-payment: monthly-payment,
        total-payments: total-payments,
        payments-made: u0,
        property-id: property-id,
        interest-rate: interest-rate,
        start-date: start-date,
        payment-interval: payment-interval,
        status: "active",
        last-payment-date: u0,
        next-payment-due: next-payment-due,
        default-threshold: default-threshold
      }
    )
    
    (map-set mortgage-balances
      { mortgage-id: mortgage-id }
      {
        remaining-principal: total-amount,
        total-interest-paid: u0,
        total-late-fees: u0,
        escrow-balance: u0
      }
    )
    
    (var-set mortgage-counter mortgage-id)
    (ok mortgage-id)
  )
)

(define-public (make-mortgage-payment (mortgage-id uint))
  (let
    (
      (mortgage (unwrap! (map-get? mortgages { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (balance (unwrap! (map-get? mortgage-balances { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (current-payment-number (+ (get payments-made mortgage) u1))
      (payment-amount (get monthly-payment mortgage))
      (interest-portion (/ (* (get remaining-principal balance) (get interest-rate mortgage)) u10000))
      (principal-portion (- payment-amount interest-portion))
      (new-remaining-balance (- (get remaining-principal balance) principal-portion))
      (late-fee (if (> stacks-block-height (get next-payment-due mortgage)) u50 u0))
      (total-payment (+ payment-amount late-fee))
    )
    (asserts! (is-eq tx-sender (get borrower mortgage)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status mortgage) "active") ERR_MORTGAGE_COMPLETED)
    (asserts! (< (get payments-made mortgage) (get total-payments mortgage)) ERR_MORTGAGE_COMPLETED)
    (asserts! (is-none (map-get? mortgage-payments { mortgage-id: mortgage-id, payment-number: current-payment-number })) ERR_PAYMENT_ALREADY_MADE)
    
    (try! (stx-transfer? total-payment tx-sender (get lender mortgage)))
    
    (map-set mortgage-payments
      { mortgage-id: mortgage-id, payment-number: current-payment-number }
      {
        amount: payment-amount,
        payment-date: stacks-block-height,
        principal-portion: principal-portion,
        interest-portion: interest-portion,
        remaining-balance: new-remaining-balance,
        late-fee: late-fee,
        status: "paid"
      }
    )
    
    (map-set mortgage-balances
      { mortgage-id: mortgage-id }
      {
        remaining-principal: new-remaining-balance,
        total-interest-paid: (+ (get total-interest-paid balance) interest-portion),
        total-late-fees: (+ (get total-late-fees balance) late-fee),
        escrow-balance: (get escrow-balance balance)
      }
    )
    
    (let
      (
        (updated-mortgage (merge mortgage {
          payments-made: current-payment-number,
          last-payment-date: stacks-block-height,
          next-payment-due: (+ stacks-block-height (get payment-interval mortgage)),
          status: (if (is-eq current-payment-number (get total-payments mortgage)) "completed" "active")
        }))
      )
      (map-set mortgages { mortgage-id: mortgage-id } updated-mortgage)
    )
    
    (ok current-payment-number)
  )
)

(define-public (check-mortgage-default (mortgage-id uint))
  (let
    (
      (mortgage (unwrap! (map-get? mortgages { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (overdue-blocks (- stacks-block-height (get next-payment-due mortgage)))
    )
    (asserts! (is-eq (get status mortgage) "active") ERR_MORTGAGE_COMPLETED)
    (asserts! (> overdue-blocks (get default-threshold mortgage)) ERR_PAYMENT_NOT_DUE)
    
    (map-set mortgages
      { mortgage-id: mortgage-id }
      (merge mortgage { status: "defaulted" })
    )
    (ok true)
  )
)

(define-public (calculate-early-payoff (mortgage-id uint))
  (let
    (
      (mortgage (unwrap! (map-get? mortgages { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (balance (unwrap! (map-get? mortgage-balances { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (remaining-payments (- (get total-payments mortgage) (get payments-made mortgage)))
      (future-interest (* remaining-payments (/ (* (get remaining-principal balance) (get interest-rate mortgage)) u10000)))
      (discount-factor u9000)
      (discounted-interest (/ (* future-interest discount-factor) u10000))
      (payoff-amount (+ (get remaining-principal balance) discounted-interest))
    )
    (asserts! (is-eq (get status mortgage) "active") ERR_MORTGAGE_COMPLETED)
    (asserts! (> remaining-payments u0) ERR_MORTGAGE_COMPLETED)
    
    (ok payoff-amount)
  )
)

(define-public (make-early-payoff (mortgage-id uint))
  (let
    (
      (mortgage (unwrap! (map-get? mortgages { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (balance (unwrap! (map-get? mortgage-balances { mortgage-id: mortgage-id }) ERR_MORTGAGE_NOT_FOUND))
      (payoff-amount (unwrap! (calculate-early-payoff mortgage-id) ERR_MORTGAGE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get borrower mortgage)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status mortgage) "active") ERR_MORTGAGE_COMPLETED)
    
    (try! (stx-transfer? payoff-amount tx-sender (get lender mortgage)))
    
    (map-set mortgages
      { mortgage-id: mortgage-id }
      (merge mortgage { 
        status: "paid-off",
        last-payment-date: stacks-block-height
      })
    )
    
    (map-set mortgage-balances
      { mortgage-id: mortgage-id }
      (merge balance { remaining-principal: u0 })
    )
    
    (ok true)
  )
)

(define-read-only (get-mortgage (mortgage-id uint))
  (map-get? mortgages { mortgage-id: mortgage-id })
)

(define-read-only (get-mortgage-balance (mortgage-id uint))
  (map-get? mortgage-balances { mortgage-id: mortgage-id })
)

(define-read-only (get-mortgage-payment (mortgage-id uint) (payment-number uint))
  (map-get? mortgage-payments { mortgage-id: mortgage-id, payment-number: payment-number })
)

(define-read-only (get-mortgage-count)
  (var-get mortgage-counter)
)

(define-read-only (is-payment-overdue (mortgage-id uint))
  (match (map-get? mortgages { mortgage-id: mortgage-id })
    mortgage (and 
      (is-eq (get status mortgage) "active")
      (> stacks-block-height (get next-payment-due mortgage))
    )
    false
  )
)

(define-read-only (get-next-payment-amount (mortgage-id uint))
  (match (map-get? mortgages { mortgage-id: mortgage-id })
    mortgage (let
      (
        (base-payment (get monthly-payment mortgage))
        (late-fee (if (> stacks-block-height (get next-payment-due mortgage)) u50 u0))
      )
      (ok (+ base-payment late-fee))
    )
    ERR_MORTGAGE_NOT_FOUND
  )
)

(define-read-only (get-mortgage-summary (mortgage-id uint))
  (match (map-get? mortgages { mortgage-id: mortgage-id })
    mortgage (match (map-get? mortgage-balances { mortgage-id: mortgage-id })
      balance (ok {
        mortgage-id: mortgage-id,
        borrower: (get borrower mortgage),
        lender: (get lender mortgage),
        property-id: (get property-id mortgage),
        status: (get status mortgage),
        payments-made: (get payments-made mortgage),
        total-payments: (get total-payments mortgage),
        remaining-principal: (get remaining-principal balance),
        next-payment-due: (get next-payment-due mortgage),
        monthly-payment: (get monthly-payment mortgage),
        total-interest-paid: (get total-interest-paid balance),
        is-overdue: (> stacks-block-height (get next-payment-due mortgage))
      })
      ERR_MORTGAGE_NOT_FOUND
    )
    ERR_MORTGAGE_NOT_FOUND
  )
)

(define-public (register-inspector (inspector principal) (license-number (string-ascii 32)) (specialty (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set inspectors 
      { inspector: inspector }
      {
        authorized: true,
        license-number: license-number,
        specialty: specialty,
        rating: u5,
        total-inspections: u0
      }
    )
    (ok true)
  )
)

(define-public (revoke-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (map-get? inspectors { inspector: inspector })) ERR_INSPECTOR_NOT_AUTHORIZED)
    (map-set inspectors 
      { inspector: inspector }
      (merge (unwrap-panic (map-get? inspectors { inspector: inspector })) { authorized: false })
    )
    (ok true)
  )
)

(define-public (schedule-inspection (property-id (string-ascii 64)) (escrow-id uint) (inspector principal) (inspection-type (string-ascii 32)) (scheduled-date uint))
  (let
    (
      (inspection-id (+ (var-get inspection-counter) u1))
      (inspector-info (unwrap! (map-get? inspectors { inspector: inspector }) ERR_INSPECTOR_NOT_AUTHORIZED))
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (get authorized inspector-info) ERR_INSPECTOR_NOT_AUTHORIZED)
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_BUYER_OR_SELLER)
    (asserts! (is-none (map-get? property-inspections { inspection-id: inspection-id })) ERR_INSPECTION_ALREADY_EXISTS)
    
    (map-set property-inspections
      { inspection-id: inspection-id }
      {
        property-id: property-id,
        escrow-id: escrow-id,
        inspector: inspector,
        inspection-type: inspection-type,
        scheduled-date: scheduled-date,
        completed-date: u0,
        status: "scheduled",
        overall-rating: u0,
        total-findings: u0,
        critical-findings: u0,
        estimated-repair-cost: u0
      }
    )
    
    (var-set inspection-counter inspection-id)
    (ok inspection-id)
  )
)

(define-public (complete-inspection (inspection-id uint) (overall-rating uint) (summary (string-ascii 256)) (recommendations (string-ascii 256)) (pass-fail-status (string-ascii 20)))
  (let
    (
      (inspection (unwrap! (map-get? property-inspections { inspection-id: inspection-id }) ERR_INSPECTION_NOT_FOUND))
      (inspector-info (unwrap! (map-get? inspectors { inspector: (get inspector inspection) }) ERR_INSPECTOR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq tx-sender (get inspector inspection)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status inspection) "scheduled") ERR_INSPECTION_COMPLETED)
    (asserts! (<= overall-rating u10) ERR_INVALID_SEVERITY)
    
    (map-set property-inspections
      { inspection-id: inspection-id }
      (merge inspection {
        completed-date: stacks-block-height,
        status: "completed",
        overall-rating: overall-rating
      })
    )
    
    (map-set inspection-reports
      { inspection-id: inspection-id }
      {
        summary: summary,
        recommendations: recommendations,
        pass-fail-status: pass-fail-status,
        inspector-notes: "",
        buyer-acknowledged: false,
        seller-acknowledged: false
      }
    )
    
    (map-set inspectors
      { inspector: (get inspector inspection) }
      (merge inspector-info { total-inspections: (+ (get total-inspections inspector-info) u1) })
    )
    
    (ok true)
  )
)

(define-public (add-inspection-finding (inspection-id uint) (area (string-ascii 32)) (finding-type (string-ascii 32)) (severity uint) (description (string-ascii 128)) (estimated-cost uint) (repair-required bool))
  (let
    (
      (finding-id (+ (var-get finding-counter) u1))
      (inspection (unwrap! (map-get? property-inspections { inspection-id: inspection-id }) ERR_INSPECTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get inspector inspection)) ERR_NOT_AUTHORIZED)
    (asserts! (>= severity u1) ERR_INVALID_SEVERITY)
    (asserts! (<= severity u5) ERR_INVALID_SEVERITY)
    
    (map-set inspection-findings
      { finding-id: finding-id }
      {
        inspection-id: inspection-id,
        area: area,
        finding-type: finding-type,
        severity: severity,
        description: description,
        estimated-cost: estimated-cost,
        photo-hash: "",
        repair-required: repair-required,
        repair-completed: false,
        repair-verified-by: none,
        repair-completion-date: u0
      }
    )
    
    (let
      (
        (current-total (get total-findings inspection))
        (current-critical (get critical-findings inspection))
        (current-cost (get estimated-repair-cost inspection))
        (new-critical (if (>= severity u4) (+ current-critical u1) current-critical))
      )
      (map-set property-inspections
        { inspection-id: inspection-id }
        (merge inspection {
          total-findings: (+ current-total u1),
          critical-findings: new-critical,
          estimated-repair-cost: (+ current-cost estimated-cost)
        })
      )
    )
    
    (var-set finding-counter finding-id)
    (ok finding-id)
  )
)

(define-public (mark-repair-completed (finding-id uint) (verifier principal))
  (let
    (
      (finding (unwrap! (map-get? inspection-findings { finding-id: finding-id }) ERR_FINDING_NOT_FOUND))
      (inspection (unwrap! (map-get? property-inspections { inspection-id: (get inspection-id finding) }) ERR_INSPECTION_NOT_FOUND))
      (escrow (unwrap! (map-get? escrows { escrow-id: (get escrow-id inspection) }) ERR_ESCROW_NOT_FOUND))
      (verifier-info (default-to { authorized: false } (map-get? verifiers { verifier: verifier })))
    )
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow))
      (is-eq tx-sender (get seller escrow))
      (and (is-eq tx-sender verifier) (get authorized verifier-info))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (get repair-required finding) ERR_REPAIR_ALREADY_COMPLETED)
    (asserts! (not (get repair-completed finding)) ERR_REPAIR_ALREADY_COMPLETED)
    
    (map-set inspection-findings
      { finding-id: finding-id }
      (merge finding {
        repair-completed: true,
        repair-verified-by: (some verifier),
        repair-completion-date: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (acknowledge-inspection-report (inspection-id uint) (is-buyer bool))
  (let
    (
      (inspection (unwrap! (map-get? property-inspections { inspection-id: inspection-id }) ERR_INSPECTION_NOT_FOUND))
      (escrow (unwrap! (map-get? escrows { escrow-id: (get escrow-id inspection) }) ERR_ESCROW_NOT_FOUND))
      (report (unwrap! (map-get? inspection-reports { inspection-id: inspection-id }) ERR_INSPECTION_NOT_FOUND))
    )
    (if is-buyer
      (begin
        (asserts! (is-eq tx-sender (get buyer escrow)) ERR_NOT_AUTHORIZED)
        (map-set inspection-reports
          { inspection-id: inspection-id }
          (merge report { buyer-acknowledged: true })
        )
      )
      (begin
        (asserts! (is-eq tx-sender (get seller escrow)) ERR_NOT_AUTHORIZED)
        (map-set inspection-reports
          { inspection-id: inspection-id }
          (merge report { seller-acknowledged: true })
        )
      )
    )
    (ok true)
  )
)

(define-public (update-inspector-rating (inspector principal) (new-rating uint))
  (let
    (
      (inspector-info (unwrap! (map-get? inspectors { inspector: inspector }) ERR_INSPECTOR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rating u10) ERR_INVALID_SEVERITY)
    
    (map-set inspectors
      { inspector: inspector }
      (merge inspector-info { rating: new-rating })
    )
    (ok true)
  )
)

(define-read-only (get-inspector-info (inspector principal))
  (map-get? inspectors { inspector: inspector })
)

(define-read-only (get-property-inspection (inspection-id uint))
  (map-get? property-inspections { inspection-id: inspection-id })
)

(define-read-only (get-inspection-report (inspection-id uint))
  (map-get? inspection-reports { inspection-id: inspection-id })
)

(define-read-only (get-inspection-finding (finding-id uint))
  (map-get? inspection-findings { finding-id: finding-id })
)

(define-read-only (get-inspection-count)
  (var-get inspection-counter)
)

(define-read-only (get-finding-count)
  (var-get finding-counter)
)

(define-read-only (is-inspector-authorized (inspector principal))
  (match (map-get? inspectors { inspector: inspector })
    inspector-info (get authorized inspector-info)
    false
  )
)

(define-read-only (get-escrow-inspections (escrow-id uint))
  (let
    (
      (inspection-count (var-get inspection-counter))
    )
    (ok escrow-id)
  )
)

(define-read-only (are-critical-repairs-completed (inspection-id uint))
  (let
    (
      (inspection (unwrap! (map-get? property-inspections { inspection-id: inspection-id }) (err false)))
      (finding-count (var-get finding-counter))
    )
    (ok true)
  )
)


