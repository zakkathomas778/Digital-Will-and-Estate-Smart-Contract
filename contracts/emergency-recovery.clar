(define-constant ERR-NOT-TRUSTED-CONTACT (err u200))
(define-constant ERR-CHALLENGE-EXISTS (err u201))
(define-constant ERR-NO-CHALLENGE (err u202))
(define-constant ERR-CHALLENGE-PERIOD-ACTIVE (err u203))
(define-constant ERR-INSUFFICIENT-CHALLENGES (err u204))
(define-constant ERR-CONTACT-EXISTS (err u205))

(define-constant CHALLENGE-PERIOD u144)
(define-constant MIN-TRUSTED-CONTACTS u2)

(define-map trusted-contacts
    { will-owner: principal, contact: principal }
    { active: bool, added-at: uint }
)

(define-map emergency-challenges
    principal
    {
        challenge-height: uint,
        challenger-count: uint,
        responded: bool,
        activated: bool
    }
)

(define-map challenge-participants
    { will-owner: principal, challenger: principal }
    { participated: bool }
)

(define-map will-owner-contacts
    principal
    { contact-count: uint }
)

(define-read-only (get-trusted-contact (will-owner principal) (contact principal))
    (map-get? trusted-contacts { will-owner: will-owner, contact: contact })
)

(define-read-only (get-emergency-challenge (will-owner principal))
    (map-get? emergency-challenges will-owner)
)

(define-read-only (get-contact-count (will-owner principal))
    (default-to { contact-count: u0 } (map-get? will-owner-contacts will-owner))
)

(define-public (add-trusted-contact (contact principal))
    (let (
        (current-count (get contact-count (get-contact-count tx-sender)))
    )
        (asserts! (is-none (map-get? trusted-contacts { will-owner: tx-sender, contact: contact })) ERR-CONTACT-EXISTS)
        (map-set trusted-contacts { will-owner: tx-sender, contact: contact }
            { active: true, added-at: stacks-block-height }
        )
        (map-set will-owner-contacts tx-sender { contact-count: (+ current-count u1) })
        (ok true)
    )
)

(define-public (remove-trusted-contact (contact principal))
    (let (
        (contact-data (unwrap! (map-get? trusted-contacts { will-owner: tx-sender, contact: contact }) ERR-NOT-TRUSTED-CONTACT))
        (current-count (get contact-count (get-contact-count tx-sender)))
    )
        (map-delete trusted-contacts { will-owner: tx-sender, contact: contact })
        (map-set will-owner-contacts tx-sender { contact-count: (- current-count u1) })
        (ok true)
    )
)

(define-public (initiate-emergency-challenge (will-owner principal))
    (let (
        (contact-data (unwrap! (map-get? trusted-contacts { will-owner: will-owner, contact: tx-sender }) ERR-NOT-TRUSTED-CONTACT))
        (existing-challenge (map-get? emergency-challenges will-owner))
    )
        (asserts! (get active contact-data) ERR-NOT-TRUSTED-CONTACT)
        (asserts! (is-none existing-challenge) ERR-CHALLENGE-EXISTS)
        (map-set emergency-challenges will-owner
            {
                challenge-height: stacks-block-height,
                challenger-count: u1,
                responded: false,
                activated: false
            }
        )
        (map-set challenge-participants { will-owner: will-owner, challenger: tx-sender }
            { participated: true }
        )
        (ok true)
    )
)

(define-public (support-emergency-challenge (will-owner principal))
    (let (
        (contact-data (unwrap! (map-get? trusted-contacts { will-owner: will-owner, contact: tx-sender }) ERR-NOT-TRUSTED-CONTACT))
        (challenge (unwrap! (map-get? emergency-challenges will-owner) ERR-NO-CHALLENGE))
        (already-participated (default-to { participated: false } (map-get? challenge-participants { will-owner: will-owner, challenger: tx-sender })))
    )
        (asserts! (get active contact-data) ERR-NOT-TRUSTED-CONTACT)
        (asserts! (not (get participated already-participated)) ERR-CHALLENGE-EXISTS)
        (asserts! (not (get responded challenge)) ERR-CHALLENGE-PERIOD-ACTIVE)
        (map-set challenge-participants { will-owner: will-owner, challenger: tx-sender }
            { participated: true }
        )
        (map-set emergency-challenges will-owner
            (merge challenge { challenger-count: (+ (get challenger-count challenge) u1) })
        )
        (ok true)
    )
)

(define-public (respond-to-challenge)
    (let (
        (challenge (unwrap! (map-get? emergency-challenges tx-sender) ERR-NO-CHALLENGE))
    )
        (asserts! (not (get responded challenge)) ERR-CHALLENGE-PERIOD-ACTIVE)
        (asserts! (< (- stacks-block-height (get challenge-height challenge)) CHALLENGE-PERIOD) ERR-CHALLENGE-PERIOD-ACTIVE)
        (map-set emergency-challenges tx-sender
            (merge challenge { responded: true })
        )
        (ok true)
    )
)

(define-public (activate-emergency-will (will-owner principal))
    (let (
        (challenge (unwrap! (map-get? emergency-challenges will-owner) ERR-NO-CHALLENGE))
        (contact-count (get contact-count (get-contact-count will-owner)))
    )
        (asserts! (>= (get challenger-count challenge) MIN-TRUSTED-CONTACTS) ERR-INSUFFICIENT-CHALLENGES)
        (asserts! (>= (- stacks-block-height (get challenge-height challenge)) CHALLENGE-PERIOD) ERR-CHALLENGE-PERIOD-ACTIVE)
        (asserts! (not (get responded challenge)) ERR-CHALLENGE-PERIOD-ACTIVE)
        (asserts! (not (get activated challenge)) ERR-CHALLENGE-EXISTS)
        (map-set emergency-challenges will-owner
            (merge challenge { activated: true })
        )
        (ok true)
    )
)

(define-read-only (can-claim-via-emergency (will-owner principal))
    (match (map-get? emergency-challenges will-owner)
        challenge (and 
            (get activated challenge)
            (not (get responded challenge))
            (>= (- stacks-block-height (get challenge-height challenge)) CHALLENGE-PERIOD)
        )
        false
    )
)

(define-public (emergency-claim-inheritance (will-owner principal))
    (let (
        (challenge (unwrap! (map-get? emergency-challenges will-owner) ERR-NO-CHALLENGE))
    )
        (asserts! (can-claim-via-emergency will-owner) ERR-CHALLENGE-PERIOD-ACTIVE)
        (ok true)
    )
)