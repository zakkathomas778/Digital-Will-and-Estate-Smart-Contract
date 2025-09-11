;; Will Amendment History Tracker
;; Tracks versioned changes to wills for audit trail and legal compliance

(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-WILL-NOT-FOUND (err u501))
(define-constant ERR-AMENDMENT-NOT-FOUND (err u502))
(define-constant ERR-WILL-LOCKED (err u503))

;; Track will amendment versions
(define-map will-versions
    principal
    {
        current-version: uint,
        last-amended: uint,
        total-amendments: uint,
        locked: bool
    }
)

;; Store individual amendment records
(define-map amendment-records
    { will-owner: principal, version: uint }
    {
        amendment-type: (string-ascii 30),
        amendment-date: uint,
        amended-by: principal,
        reason: (string-ascii 200),
        previous-value: (string-ascii 100),
        new-value: (string-ascii 100),
        beneficiary-affected: (optional principal)
    }
)

;; Read-only functions

(define-read-only (get-will-version-info (will-owner principal))
    (map-get? will-versions will-owner)
)

(define-read-only (get-amendment-record (will-owner principal) (version uint))
    (map-get? amendment-records { will-owner: will-owner, version: version })
)

(define-read-only (get-amendment-history (will-owner principal))
    (let ((version-info (map-get? will-versions will-owner)))
        (match version-info
            info (ok {
                current-version: (get current-version info),
                total-amendments: (get total-amendments info),
                last-amended: (get last-amended info),
                locked: (get locked info)
            })
            (err ERR-WILL-NOT-FOUND)
        )
    )
)

;; Public functions

(define-public (initialize-will-versioning (will-owner principal))
    (begin
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? will-versions will-owner)) ERR-WILL-NOT-FOUND)
        (map-set will-versions will-owner {
            current-version: u1,
            last-amended: stacks-block-height,
            total-amendments: u0,
            locked: false
        })
        (ok true)
    )
)

(define-public (record-amendment 
    (will-owner principal)
    (amendment-type (string-ascii 30))
    (reason (string-ascii 200))
    (previous-value (string-ascii 100))
    (new-value (string-ascii 100))
    (beneficiary-affected (optional principal)))
    (let (
        (version-info (unwrap! (map-get? will-versions will-owner) ERR-WILL-NOT-FOUND))
        (new-version (+ (get current-version version-info) u1))
    )
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (asserts! (not (get locked version-info)) ERR-WILL-LOCKED)
        
        ;; Record the amendment
        (map-set amendment-records { will-owner: will-owner, version: new-version }
            {
                amendment-type: amendment-type,
                amendment-date: stacks-block-height,
                amended-by: tx-sender,
                reason: reason,
                previous-value: previous-value,
                new-value: new-value,
                beneficiary-affected: beneficiary-affected
            }
        )
        
        ;; Update version info
        (map-set will-versions will-owner {
            current-version: new-version,
            last-amended: stacks-block-height,
            total-amendments: (+ (get total-amendments version-info) u1),
            locked: (get locked version-info)
        })
        
        (ok new-version)
    )
)

(define-public (lock-will-amendments (will-owner principal))
    (let ((version-info (unwrap! (map-get? will-versions will-owner) ERR-WILL-NOT-FOUND)))
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (map-set will-versions will-owner
            (merge version-info { locked: true })
        )
        (ok true)
    )
)

(define-public (unlock-will-amendments (will-owner principal))
    (let ((version-info (unwrap! (map-get? will-versions will-owner) ERR-WILL-NOT-FOUND)))
        (asserts! (is-eq tx-sender will-owner) ERR-NOT-AUTHORIZED)
        (map-set will-versions will-owner
            (merge version-info { locked: false })
        )
        (ok true)
    )
)

(define-public (compare-amendment-versions (will-owner principal) (version-a uint) (version-b uint))
    (let (
        (amendment-a (unwrap! (map-get? amendment-records { will-owner: will-owner, version: version-a }) ERR-AMENDMENT-NOT-FOUND))
        (amendment-b (unwrap! (map-get? amendment-records { will-owner: will-owner, version: version-b }) ERR-AMENDMENT-NOT-FOUND))
    )
        (ok {
            version-a-info: amendment-a,
            version-b-info: amendment-b,
            time-difference: (- (get amendment-date amendment-b) (get amendment-date amendment-a)),
            same-type: (is-eq (get amendment-type amendment-a) (get amendment-type amendment-b))
        })
    )
)

