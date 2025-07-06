(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INSUFFICIENT-BALANCE (err u301))
(define-constant ERR-CUSTODY-NOT-FOUND (err u302))
(define-constant ERR-CUSTODY-LOCKED (err u303))
(define-constant ERR-INVALID-AMOUNT (err u304))
(define-constant ERR-DEATH-NOT-VERIFIED (err u305))
(define-constant ERR-ALREADY-CLAIMED (err u306))
(define-constant ERR-WILL-NOT-FOUND (err u307))
(define-constant ERR-EMERGENCY-ACTIVE (err u308))



(define-map asset-custody
    principal
    {
        stx-balance: uint,
        locked: bool,
        deposit-height: uint,
        release-height: uint
    }
)

(define-map custody-beneficiaries
    { will-owner: principal, beneficiary: principal }
    {
        stx-claimed: bool,
        claim-height: uint
    }
)

(define-data-var contract-owner principal tx-sender)

(define-read-only (get-custody-details (owner principal))
    (match (map-get? asset-custody owner)
        custody-data (ok custody-data)
        (err ERR-CUSTODY-NOT-FOUND)
    )
)

(define-read-only (get-beneficiary-claim-status (will-owner principal) (beneficiary principal))
    (match (map-get? custody-beneficiaries { will-owner: will-owner, beneficiary: beneficiary })
        claim-data (ok claim-data)
        (ok { stx-claimed: false, claim-height: u0 })
    )
)

(define-private (calculate-beneficiary-amount (will-owner principal) (beneficiary principal) (allocation uint))
    (let (
        (custody (unwrap! (map-get? asset-custody will-owner) ERR-CUSTODY-NOT-FOUND))
    )
        (ok (/ (* (get stx-balance custody) allocation) u100))
    )
)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-public (deposit-stx (amount uint))
    (let (
        (current-custody (default-to { stx-balance: u0, locked: false, deposit-height: u0, release-height: u0 } 
                                   (map-get? asset-custody tx-sender)))
        (new-balance (+ (get stx-balance current-custody) amount))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set asset-custody tx-sender {
            stx-balance: new-balance,
            locked: (get locked current-custody),
            deposit-height: stacks-block-height,
            release-height: (get release-height current-custody)
        })
        (ok true)
    )
)

(define-public (withdraw-stx (amount uint))
    (let (
        (custody (unwrap! (map-get? asset-custody tx-sender) ERR-CUSTODY-NOT-FOUND))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= (get stx-balance custody) amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (not (get locked custody)) ERR-CUSTODY-LOCKED)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set asset-custody tx-sender 
            (merge custody { stx-balance: (- (get stx-balance custody) amount) })
        )
        (ok true)
    )
)

(define-public (lock-custody (will-owner principal))
    (let (
        (custody (unwrap! (map-get? asset-custody will-owner) ERR-CUSTODY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set asset-custody will-owner 
            (merge custody { 
                locked: true,
                release-height: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (claim-inheritance-stx (will-owner principal) (allocation uint))
    (let (
        (custody (unwrap! (map-get? asset-custody will-owner) ERR-CUSTODY-NOT-FOUND))
        (claim-status (unwrap! (get-beneficiary-claim-status will-owner tx-sender) ERR-CUSTODY-NOT-FOUND))
        (claim-amount (unwrap! (calculate-beneficiary-amount will-owner tx-sender allocation) ERR-WILL-NOT-FOUND))
    )
        (asserts! (get locked custody) ERR-CUSTODY-LOCKED)
        (asserts! (not (get stx-claimed claim-status)) ERR-ALREADY-CLAIMED)
        (try! (as-contract (stx-transfer? claim-amount tx-sender tx-sender)))
        (map-set custody-beneficiaries { will-owner: will-owner, beneficiary: tx-sender } {
            stx-claimed: true,
            claim-height: stacks-block-height
        })
        (map-set asset-custody will-owner
            (merge custody { stx-balance: (- (get stx-balance custody) claim-amount) })
        )
        (ok claim-amount)
    )
)

(define-public (emergency-unlock (will-owner principal))
    (let (
        (custody (unwrap! (map-get? asset-custody will-owner) ERR-CUSTODY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set asset-custody will-owner 
            (merge custody { 
                locked: true,
                release-height: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-read-only (get-total-custody-balance)
    (stx-get-balance (as-contract tx-sender))
)

(define-read-only (is-custody-ready-for-claims (will-owner principal))
    (match (map-get? asset-custody will-owner)
        custody (get locked custody)
        false
    )
)
