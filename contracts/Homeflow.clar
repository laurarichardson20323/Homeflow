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

(define-data-var escrow-counter uint u0)

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