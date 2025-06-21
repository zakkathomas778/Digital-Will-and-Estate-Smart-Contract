
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-NOT-INITIALIZED (err u102))
(define-constant ERR-INVALID-BENEFICIARY (err u103))
(define-constant ERR-WILL-EXISTS (err u104))
(define-constant ERR-NO-WILL (err u105))
(define-constant ERR-INVALID-ALLOCATION (err u106))
(define-constant ERR-DEATH-NOT-VERIFIED (err u107))

(define-data-var contract-owner principal tx-sender)
(define-data-var executor (optional principal) none)
(define-data-var contract-initialized bool false)

(define-map wills
    principal
    {
        active: bool,
        death-verified: bool,
        total-allocation: uint,
        assets: uint
    }
)

(define-map beneficiaries
    { will-owner: principal, beneficiary: principal }
    {
        allocation: uint,
        claimed: bool
    }
)

(define-read-only (get-will-details (owner principal))
    (match (map-get? wills owner)
        will-data (ok will-data)
        (err ERR-NO-WILL)
    )
)

(define-read-only (get-beneficiary-details (will-owner principal) (beneficiary principal))
    (match (map-get? beneficiaries { will-owner: will-owner, beneficiary: beneficiary })
        beneficiary-data (ok beneficiary-data)
        (err ERR-INVALID-BENEFICIARY)
    )
)

(define-public (initialize-contract (executor-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (var-get contract-initialized)) ERR-ALREADY-INITIALIZED)
        (var-set executor (some executor-address))
        (var-set contract-initialized true)
        (ok true)
    )
)

(define-public (create-will)
    (begin
        (asserts! (var-get contract-initialized) ERR-NOT-INITIALIZED)
        (asserts! (is-none (map-get? wills tx-sender)) ERR-WILL-EXISTS)
        (map-set wills tx-sender {
            active: true,
            death-verified: false,
            total-allocation: u0,
            assets: u0
        })
        (ok true)
    )
)

(define-public (add-beneficiary (beneficiary principal) (allocation uint))
    (let (
        (will (unwrap! (map-get? wills tx-sender) ERR-NO-WILL))
        (new-total (+ allocation (get total-allocation will)))
    )
        (asserts! (<= new-total u100) ERR-INVALID-ALLOCATION)
        (map-set beneficiaries { will-owner: tx-sender, beneficiary: beneficiary }
            {
                allocation: allocation,
                claimed: false
            }
        )
        (map-set wills tx-sender (merge will { total-allocation: new-total }))
        (ok true)
    )
)

(define-public (remove-beneficiary (beneficiary principal))
    (let (
        (will (unwrap! (map-get? wills tx-sender) ERR-NO-WILL))
        (beneficiary-data (unwrap! (map-get? beneficiaries { will-owner: tx-sender, beneficiary: beneficiary }) ERR-INVALID-BENEFICIARY))
    )
        (map-delete beneficiaries { will-owner: tx-sender, beneficiary: beneficiary })
        (map-set wills tx-sender (merge will { total-allocation: (- (get total-allocation will) (get allocation beneficiary-data)) }))
        (ok true)
    )
)

(define-public (verify-death (will-owner principal))
    (let ((executor-address (unwrap! (var-get executor) ERR-NOT-INITIALIZED)))
        (asserts! (is-eq tx-sender executor-address) ERR-NOT-AUTHORIZED)
        (match (map-get? wills will-owner)
            will-data (begin
                (map-set wills will-owner (merge will-data { death-verified: true }))
                (ok true)
            )
            ERR-NO-WILL
        )
    )
)

(define-public (deposit-assets (amount uint))
    (let ((will (unwrap! (map-get? wills tx-sender) ERR-NO-WILL)))
        (map-set wills tx-sender (merge will { assets: (+ (get assets will) amount) }))
        (ok true)
    )
)

(define-public (claim-inheritance (will-owner principal))
    (let (
        (will (unwrap! (map-get? wills will-owner) ERR-NO-WILL))
        (beneficiary-data (unwrap! (map-get? beneficiaries { will-owner: will-owner, beneficiary: tx-sender }) ERR-INVALID-BENEFICIARY))
    )
        (asserts! (get death-verified will) ERR-DEATH-NOT-VERIFIED)
        (asserts! (not (get claimed beneficiary-data)) ERR-INVALID-BENEFICIARY)
        (let (
            (claim-amount (/ (* (get assets will) (get allocation beneficiary-data)) u100))
        )
            (map-set beneficiaries { will-owner: will-owner, beneficiary: tx-sender }
                (merge beneficiary-data { claimed: true })
            )
            (ok claim-amount)
        )
    )
)

(define-public (update-executor (new-executor principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set executor (some new-executor))
        (ok true)
    )
)


(define-constant ERR-WILL-EXPIRED (err u108))

(define-map will-expiration
    principal 
    { expiration-height: uint }
)

(define-public (set-will-expiration (expiration-height uint))
    (let ((will (unwrap! (map-get? wills tx-sender) ERR-NO-WILL)))
        (asserts! (> expiration-height stacks-block-height) ERR-INVALID-ALLOCATION)
        (map-set will-expiration tx-sender { expiration-height: expiration-height })
        (ok true)
    )
)

(define-read-only (get-will-expiration (owner principal))
    (match (map-get? will-expiration owner)
        expiration-data (ok expiration-data)
        (err ERR-NO-WILL)
    )
)

(define-constant ERR-EXECUTOR-EXISTS (err u109))
(define-constant ERR-INSUFFICIENT-CONFIRMATIONS (err u110))

(define-map executors
    principal
    {
        active: bool,
        confirmations: uint,
        required-confirmations: uint
    }
)

(define-map executor-list
    { will-owner: principal, executor: principal }
    { confirmed: bool }
)

(define-public (add-executor (executor-principal principal))
    (let ((will (unwrap! (map-get? wills tx-sender) ERR-NO-WILL)))
        (asserts! (is-none (map-get? executor-list { will-owner: tx-sender, executor: executor-principal })) ERR-EXECUTOR-EXISTS)
        (map-set executor-list { will-owner: tx-sender, executor: executor-principal } { confirmed: false })
        (ok true)
    )
)

(define-public (confirm-death (will-owner principal))
    (let (
        (executor-data (unwrap! (map-get? executor-list { will-owner: will-owner, executor: tx-sender }) ERR-NOT-AUTHORIZED))
        (will (unwrap! (map-get? wills will-owner) ERR-NO-WILL))
    )
        (map-set executor-list { will-owner: will-owner, executor: tx-sender } { confirmed: true })
        (ok true)
    )
)

