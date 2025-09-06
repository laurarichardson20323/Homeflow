;; Property Appraisal System for Homeflow
;; Provides professional property valuation services integrated with escrow

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_APPRAISAL_NOT_FOUND (err u201))
(define-constant ERR_APPRAISER_NOT_FOUND (err u202))
(define-constant ERR_INVALID_VALUE (err u203))
(define-constant ERR_APPRAISAL_EXISTS (err u204))
(define-constant ERR_APPRAISAL_COMPLETED (err u205))
(define-constant ERR_ESCROW_NOT_FOUND (err u206))

;; Data variables
(define-data-var appraisal-counter uint u0)
(define-data-var contract-owner principal tx-sender)

;; Certified appraisers registry
(define-map certified-appraisers
    { appraiser: principal }
    {
        license-number: (string-ascii 32),
        certification-level: (string-ascii 20), ;; "RESIDENTIAL", "COMMERCIAL", "EXPERT"
        rating: uint, ;; 1-10 scale
        total-appraisals: uint,
        average-accuracy: uint, ;; percentage * 100
        is-active: bool
    }
)

;; Property appraisal records
(define-map property-appraisals
    { appraisal-id: uint }
    {
        property-id: (string-ascii 64),
        escrow-id: uint,
        appraiser: principal,
        requested-by: principal,
        appraised-value: uint,
        comparable-properties: (list 3 uint), ;; recent sales for comparison
        appraisal-method: (string-ascii 32), ;; "SALES_COMPARISON", "COST_APPROACH", "INCOME"
        property-type: (string-ascii 32), ;; "SINGLE_FAMILY", "CONDO", "TOWNHOUSE", "COMMERCIAL"
        square-footage: uint,
        lot-size: uint,
        year-built: uint,
        condition-rating: uint, ;; 1-10 scale
        market-trend-factor: uint, ;; percentage * 100 (100 = neutral, >100 = appreciating)
        appraisal-date: uint,
        status: (string-ascii 20), ;; "REQUESTED", "IN_PROGRESS", "COMPLETED", "DISPUTED"
        report-hash: (string-ascii 64), ;; IPFS hash of detailed report
        fee-paid: uint
    }
)

;; Appraisal fee structure
(define-map appraisal-fees
    { property-type: (string-ascii 32) }
    {
        base-fee: uint,
        rush-fee: uint, ;; additional fee for expedited service
        complex-property-fee: uint ;; for unique/difficult properties
    }
)

;; Market comparables data
(define-map market-comparables
    { property-id: (string-ascii 64) }
    {
        recent-sale-price: uint,
        sale-date: uint,
        square-footage: uint,
        property-type: (string-ascii 32),
        condition: uint,
        distance-miles: uint ;; from subject property
    }
)

;; Initialize default fees
(map-set appraisal-fees
    { property-type: "SINGLE_FAMILY" }
    { base-fee: u50000, rush-fee: u15000, complex-property-fee: u10000 } ;; 500, 150, 100 STX
)

(map-set appraisal-fees
    { property-type: "CONDO" }
    { base-fee: u40000, rush-fee: u12000, complex-property-fee: u8000 }
)

(map-set appraisal-fees
    { property-type: "COMMERCIAL" }
    { base-fee: u100000, rush-fee: u30000, complex-property-fee: u25000 }
)

;; Register a certified appraiser
(define-public (register-appraiser (appraiser principal) (license-number (string-ascii 32)) (certification-level (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
        (map-set certified-appraisers
            { appraiser: appraiser }
            {
                license-number: license-number,
                certification-level: certification-level,
                rating: u8, ;; start with good rating
                total-appraisals: u0,
                average-accuracy: u9500, ;; 95% default
                is-active: true
            }
        )
        (ok true)
    )
)

;; Request property appraisal
(define-public (request-appraisal 
    (property-id (string-ascii 64))
    (escrow-id uint)
    (appraiser principal)
    (property-type (string-ascii 32))
    (square-footage uint)
    (lot-size uint)
    (year-built uint)
    (is-rush bool))
    (let
        (
            (appraisal-id (+ (var-get appraisal-counter) u1))
            (appraiser-info (unwrap! (map-get? certified-appraisers { appraiser: appraiser }) ERR_APPRAISER_NOT_FOUND))
            (fee-info (unwrap! (map-get? appraisal-fees { property-type: property-type }) ERR_INVALID_VALUE))
            (total-fee (if is-rush 
                        (+ (get base-fee fee-info) (get rush-fee fee-info))
                        (get base-fee fee-info)))
        )
        (asserts! (get is-active appraiser-info) ERR_APPRAISER_NOT_FOUND)
        (asserts! (> square-footage u0) ERR_INVALID_VALUE)
        
        ;; Transfer fee to appraiser
        (try! (stx-transfer? total-fee tx-sender appraiser))
        
        (map-set property-appraisals
            { appraisal-id: appraisal-id }
            {
                property-id: property-id,
                escrow-id: escrow-id,
                appraiser: appraiser,
                requested-by: tx-sender,
                appraised-value: u0,
                comparable-properties: (list u0 u0 u0),
                appraisal-method: "SALES_COMPARISON",
                property-type: property-type,
                square-footage: square-footage,
                lot-size: lot-size,
                year-built: year-built,
                condition-rating: u7, ;; default average condition
                market-trend-factor: u10000, ;; neutral market
                appraisal-date: stacks-block-height,
                status: "REQUESTED",
                report-hash: "",
                fee-paid: total-fee
            }
        )
        
        (var-set appraisal-counter appraisal-id)
        (ok appraisal-id)
    )
)

;; Complete appraisal with valuation
(define-public (complete-appraisal
    (appraisal-id uint)
    (appraised-value uint)
    (condition-rating uint)
    (market-trend-factor uint)
    (appraisal-method (string-ascii 32))
    (report-hash (string-ascii 64)))
    (let
        (
            (appraisal (unwrap! (map-get? property-appraisals { appraisal-id: appraisal-id }) ERR_APPRAISAL_NOT_FOUND))
            (appraiser-info (unwrap! (map-get? certified-appraisers { appraiser: (get appraiser appraisal) }) ERR_APPRAISER_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender (get appraiser appraisal)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status appraisal) "REQUESTED") ERR_APPRAISAL_COMPLETED)
        (asserts! (> appraised-value u0) ERR_INVALID_VALUE)
        (asserts! (<= condition-rating u10) ERR_INVALID_VALUE)
        
        ;; Update appraisal record
        (map-set property-appraisals
            { appraisal-id: appraisal-id }
            (merge appraisal {
                appraised-value: appraised-value,
                condition-rating: condition-rating,
                market-trend-factor: market-trend-factor,
                appraisal-method: appraisal-method,
                status: "COMPLETED",
                report-hash: report-hash
            })
        )
        
        ;; Update appraiser statistics
        (map-set certified-appraisers
            { appraiser: (get appraiser appraisal) }
            (merge appraiser-info {
                total-appraisals: (+ (get total-appraisals appraiser-info) u1)
            })
        )
        
        (ok appraised-value)
    )
)

;; Add market comparable
(define-public (add-market-comparable
    (property-id (string-ascii 64))
    (sale-price uint)
    (sale-date uint)
    (square-footage uint)
    (property-type (string-ascii 32))
    (condition uint)
    (distance-miles uint))
    (begin
        (asserts! (> sale-price u0) ERR_INVALID_VALUE)
        (asserts! (> square-footage u0) ERR_INVALID_VALUE)
        (asserts! (<= condition u10) ERR_INVALID_VALUE)
        
        (map-set market-comparables
            { property-id: property-id }
            {
                recent-sale-price: sale-price,
                sale-date: sale-date,
                square-footage: square-footage,
                property-type: property-type,
                condition: condition,
                distance-miles: distance-miles
            }
        )
        (ok true)
    )
)

;; Calculate price per square foot
(define-read-only (calculate-price-per-sqft (appraisal-id uint))
    (match (map-get? property-appraisals { appraisal-id: appraisal-id })
        appraisal 
        (let
            (
                (value (get appraised-value appraisal))
                (sqft (get square-footage appraisal))
            )
            (if (and (> value u0) (> sqft u0))
                (ok (/ value sqft))
                ERR_INVALID_VALUE
            )
        )
        ERR_APPRAISAL_NOT_FOUND
    )
)

;; Get appraisal details
(define-read-only (get-appraisal (appraisal-id uint))
    (map-get? property-appraisals { appraisal-id: appraisal-id })
)

;; Get appraiser info
(define-read-only (get-appraiser-info (appraiser principal))
    (map-get? certified-appraisers { appraiser: appraiser })
)

;; Check if appraisal supports escrow value
(define-read-only (validate-escrow-value (appraisal-id uint) (escrow-value uint))
    (match (map-get? property-appraisals { appraisal-id: appraisal-id })
        appraisal
        (let
            (
                (appraised-value (get appraised-value appraisal))
                (variance-threshold u1000) ;; 10% variance allowed
                (variance (if (> escrow-value appraised-value)
                            (/ (* (- escrow-value appraised-value) u10000) appraised-value)
                            (/ (* (- appraised-value escrow-value) u10000) appraised-value)
                        ))
            )
            (ok (<= variance variance-threshold))
        )
        (err false)
    )
)

;; Get market comparable
(define-read-only (get-market-comparable (property-id (string-ascii 64)))
    (map-get? market-comparables { property-id: property-id })
)

;; Get appraisal count
(define-read-only (get-appraisal-count)
    (var-get appraisal-counter)
)
