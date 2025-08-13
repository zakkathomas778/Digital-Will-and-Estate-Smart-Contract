;; Death Attestation Contract - Multi-party verification system for death confirmation

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-ATTESTOR-NOT-FOUND (err u401))
(define-constant ERR-ATTESTOR-EXISTS (err u402))
(define-constant ERR-INSUFFICIENT-ATTESTORS (err u403))
(define-constant ERR-ALREADY-ATTESTED (err u404))
(define-constant ERR-ATTESTATION-PERIOD-EXPIRED (err u405))
(define-constant ERR-ATTESTATION-NOT-READY (err u406))
(define-constant ERR-DISPUTE-PERIOD-ACTIVE (err u407))
(define-constant ERR-INVALID-THRESHOLD (err u408))
(define-constant ERR-DEATH-NOT-CONFIRMED (err u409))
(define-constant ERR-ATTESTATION-LOCKED (err u410))

;; Attestation period constants
(define-constant ATTESTATION-WINDOW u1440) ;; 10 days in blocks
(define-constant DISPUTE-PERIOD u720) ;; 5 days for disputes
(define-constant MIN-ATTESTORS u2)
(define-constant MAX-ATTESTORS u10)

;; Map to store attestor information for each will owner
(define-map attestors
    { will-owner: principal, attestor: principal }
    {
        active: bool,
        attestor-type: (string-ascii 20), ;; "doctor", "lawyer", "family", "official"
        added-at: uint,
        weight: uint ;; voting weight (1-3)
    }
)

;; Map to track attestation attempts for each will owner
(define-map death-attestations
    principal
    {
        attestation-count: uint,
        required-threshold: uint,
        attestation-started: uint,
        dispute-count: uint,
        confirmed: bool,
        locked: bool
    }
)

;; Map to store individual attestation records
(define-map attestation-records
    { will-owner: principal, attestor: principal }
    {
        attested: bool,
        attestation-height: uint,
        evidence-hash: (buff 32), ;; hash of death certificate or evidence
        disputed: bool
    }
)

;; Map to track dispute submissions
(define-map dispute-records
    { will-owner: principal, disputer: principal }
    {
        dispute-height: uint,
        reason: (string-ascii 100),
        resolved: bool
    }
)

;; Map to count attestors per will owner
(define-map attestor-counts
    principal
    { count: uint, total-weight: uint }
)

;; Contract owner for administrative functions
(define-data-var contract-owner principal tx-sender)

;; Read-only functions

(define-read-only (get-attestor-info (will-owner principal) (attestor principal))
    (map-get? attestors { will-owner: will-owner, attestor: attestor })
)

(define-read-only (get-death-attestation-status (will-owner principal))
    (map-get? death-attestations will-owner)
)

(define-read-only (get-attestation-record (will-owner principal) (attestor principal))
    (map-get? attestation-records { will-owner: will-owner, attestor: attestor })
)

(define-read-only (get-attestor-count (will-owner principal))
    (default-to { count: u0, total-weight: u0 } (map-get? attestor-counts will-owner))
)

(define-read-only (is-death-confirmed (will-owner principal))
    (match (map-get? death-attestations will-owner)
        attestation-data (and 
            (get confirmed attestation-data)
            (not (get locked attestation-data))
            (>= (- stacks-block-height (get attestation-started attestation-data)) DISPUTE-PERIOD)
        )
        false
    )
)

(define-read-only (can-start-attestation (will-owner principal))
    (let (
        (attestor-info (get-attestor-count will-owner))
        (existing-attestation (map-get? death-attestations will-owner))
    )
        (and
            (>= (get count attestor-info) MIN-ATTESTORS)
            (is-none existing-attestation)
        )
    )
)

(define-read-only (get-required-attestations (will-owner principal))
    (let ((attestor-info (get-attestor-count will-owner)))
        (/ (* (get total-weight attestor-info) u67) u100) ;; 67% threshold
    )
)

;; Public functions

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-public (add-attestor (will-owner principal) (attestor principal) (attestor-type (string-ascii 20)) (weight uint))
    (let (
        (current-count (get-attestor-count will-owner))
        (new-count (+ (get count current-count) u1))
        (new-weight (+ (get total-weight current-count) weight))
    )
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? attestors { will-owner: will-owner, attestor: attestor })) ERR-ATTESTOR-EXISTS)
        (asserts! (<= new-count MAX-ATTESTORS) ERR-INSUFFICIENT-ATTESTORS)
        (asserts! (and (>= weight u1) (<= weight u3)) ERR-INVALID-THRESHOLD)
        (asserts! (is-none (map-get? death-attestations will-owner)) ERR-ATTESTATION-LOCKED)
        
        ;; Add attestor record
        (map-set attestors { will-owner: will-owner, attestor: attestor }
            {
                active: true,
                attestor-type: attestor-type,
                added-at: stacks-block-height,
                weight: weight
            }
        )
        
        ;; Update count
        (map-set attestor-counts will-owner { count: new-count, total-weight: new-weight })
        (ok true)
    )
)

(define-public (remove-attestor (will-owner principal) (attestor principal))
    (let (
        (attestor-info (unwrap! (map-get? attestors { will-owner: will-owner, attestor: attestor }) ERR-ATTESTOR-NOT-FOUND))
        (current-count (get-attestor-count will-owner))
        (new-count (- (get count current-count) u1))
        (new-weight (- (get total-weight current-count) (get weight attestor-info)))
    )
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? death-attestations will-owner)) ERR-ATTESTATION-LOCKED)
        
        ;; Remove attestor
        (map-delete attestors { will-owner: will-owner, attestor: attestor })
        
        ;; Update count
        (map-set attestor-counts will-owner { count: new-count, total-weight: new-weight })
        (ok true)
    )
)

(define-public (initiate-death-attestation (will-owner principal))
    (let (
        (attestor-info (get-attestor-count will-owner))
        (required-attestations (get-required-attestations will-owner))
    )
        (asserts! (can-start-attestation will-owner) ERR-ATTESTATION-NOT-READY)
        (asserts! (>= (get count attestor-info) MIN-ATTESTORS) ERR-INSUFFICIENT-ATTESTORS)
        
        ;; Create attestation record
        (map-set death-attestations will-owner
            {
                attestation-count: u0,
                required-threshold: required-attestations,
                attestation-started: stacks-block-height,
                dispute-count: u0,
                confirmed: false,
                locked: false
            }
        )
        (ok true)
    )
)

(define-public (submit-death-attestation (will-owner principal) (evidence-hash (buff 32)))
    (let (
        (attestor-info (unwrap! (map-get? attestors { will-owner: will-owner, attestor: tx-sender }) ERR-ATTESTOR-NOT-FOUND))
        (attestation-status (unwrap! (map-get? death-attestations will-owner) ERR-ATTESTATION-NOT-READY))
        (existing-record (map-get? attestation-records { will-owner: will-owner, attestor: tx-sender }))
    )
        (asserts! (get active attestor-info) ERR-ATTESTOR-NOT-FOUND)
        (asserts! (not (get confirmed attestation-status)) ERR-ATTESTATION-LOCKED)
        (asserts! (< (- stacks-block-height (get attestation-started attestation-status)) ATTESTATION-WINDOW) ERR-ATTESTATION-PERIOD-EXPIRED)
        
        ;; Check if already attested
        (match existing-record
            record (asserts! (not (get attested record)) ERR-ALREADY-ATTESTED)
            true ;; No previous record, can proceed
        )
        
        ;; Submit attestation
        (map-set attestation-records { will-owner: will-owner, attestor: tx-sender }
            {
                attested: true,
                attestation-height: stacks-block-height,
                evidence-hash: evidence-hash,
                disputed: false
            }
        )
        
        ;; Update attestation count with weighted vote
        (let ((new-count (+ (get attestation-count attestation-status) (get weight attestor-info))))
            (map-set death-attestations will-owner
                (merge attestation-status { attestation-count: new-count })
            )
            
            ;; Check if threshold reached
            (if (>= new-count (get required-threshold attestation-status))
                (map-set death-attestations will-owner
                    (merge attestation-status { 
                        attestation-count: new-count,
                        confirmed: true 
                    })
                )
                true
            )
        )
        (ok true)
    )
)

(define-public (dispute-attestation (will-owner principal) (reason (string-ascii 100)))
    (let (
        (attestation-status (unwrap! (map-get? death-attestations will-owner) ERR-ATTESTATION-NOT-READY))
        (attestor-info (map-get? attestors { will-owner: will-owner, attestor: tx-sender }))
    )
        (asserts! (get confirmed attestation-status) ERR-DEATH-NOT-CONFIRMED)
        (asserts! (< (- stacks-block-height (get attestation-started attestation-status)) (+ ATTESTATION-WINDOW DISPUTE-PERIOD)) ERR-ATTESTATION-PERIOD-EXPIRED)
        
        ;; Allow any registered attestor or the will owner to dispute
        (asserts! (or (is-eq tx-sender will-owner) (is-some attestor-info)) ERR-NOT-AUTHORIZED)
        
        ;; Record dispute
        (map-set dispute-records { will-owner: will-owner, disputer: tx-sender }
            {
                dispute-height: stacks-block-height,
                reason: reason,
                resolved: false
            }
        )
        
        ;; Increment dispute count
        (map-set death-attestations will-owner
            (merge attestation-status { 
                dispute-count: (+ (get dispute-count attestation-status) u1),
                locked: true ;; Lock until disputes resolved
            })
        )
        (ok true)
    )
)

(define-public (resolve-dispute (will-owner principal) (disputer principal) (uphold-attestation bool))
    (let (
        (dispute-info (unwrap! (map-get? dispute-records { will-owner: will-owner, disputer: disputer }) ERR-ATTESTOR-NOT-FOUND))
        (attestation-status (unwrap! (map-get? death-attestations will-owner) ERR-ATTESTATION-NOT-READY))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get resolved dispute-info)) ERR-ALREADY-ATTESTED)
        
        ;; Mark dispute as resolved
        (map-set dispute-records { will-owner: will-owner, disputer: disputer }
            (merge dispute-info { resolved: true })
        )
        
        ;; Update attestation status based on resolution
        (if uphold-attestation
            ;; Uphold - reduce dispute count and potentially unlock
            (let ((new-dispute-count (- (get dispute-count attestation-status) u1)))
                (map-set death-attestations will-owner
                    (merge attestation-status { 
                        dispute-count: new-dispute-count,
                        locked: (> new-dispute-count u0)
                    })
                )
            )
            ;; Overturn - invalidate attestation
            (map-set death-attestations will-owner
                (merge attestation-status { 
                    confirmed: false,
                    locked: false,
                    dispute-count: (- (get dispute-count attestation-status) u1)
                })
            )
        )
        (ok uphold-attestation)
    )
)

(define-public (finalize-death-confirmation (will-owner principal))
    (let (
        (attestation-status (unwrap! (map-get? death-attestations will-owner) ERR-ATTESTATION-NOT-READY))
    )
        (asserts! (get confirmed attestation-status) ERR-DEATH-NOT-CONFIRMED)
        (asserts! (not (get locked attestation-status)) ERR-DISPUTE-PERIOD-ACTIVE)
        (asserts! (>= (- stacks-block-height (get attestation-started attestation-status)) (+ ATTESTATION-WINDOW DISPUTE-PERIOD)) ERR-ATTESTATION-PERIOD-EXPIRED)
        
        ;; Mark as finalized
        (map-set death-attestations will-owner
            (merge attestation-status { locked: true })
        )
        (ok true)
    )
)
