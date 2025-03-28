;; Identity Creation Contract
;; Establishes digital IDs without requiring documents

(define-data-var last-identity-id uint u0)

;; Identity data structure
(define-map identities
  { identity-id: uint }
  {
    owner: principal,
    biometric-hash: (buff 32),
    creation-time: uint,
    status: (string-ascii 20),
    recovery-address: (optional principal)
  }
)

;; Identity metadata (optional additional information)
(define-map identity-metadata
  { identity-id: uint }
  {
    name: (optional (string-utf8 100)),
    birth-year: (optional uint),
    gender: (optional (string-ascii 20)),
    nationality: (optional (string-ascii 50)),
    languages: (optional (list 5 (string-ascii 20))),
    last-updated: uint
  }
)

;; Identity to principal mapping (for lookups)
(define-map identity-by-principal
  { owner: principal }
  { identity-id: uint }
)

;; Initialize contract with admin
(define-data-var contract-admin principal tx-sender)

;; Create a new identity
(define-public (create-identity
    (biometric-hash (buff 32))
    (recovery-address (optional principal)))
  (let
    (
      (new-id (+ (var-get last-identity-id) u1))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Check if principal already has an identity
    (asserts! (is-none (map-get? identity-by-principal { owner: tx-sender })) (err u1))

    (var-set last-identity-id new-id)
    (map-set identities
      { identity-id: new-id }
      {
        owner: tx-sender,
        biometric-hash: biometric-hash,
        creation-time: current-time,
        status: "ACTIVE",
        recovery-address: recovery-address
      }
    )

    ;; Set up empty metadata
    (map-set identity-metadata
      { identity-id: new-id }
      {
        name: none,
        birth-year: none,
        gender: none,
        nationality: none,
        languages: none,
        last-updated: current-time
      }
    )

    ;; Create reverse lookup
    (map-set identity-by-principal
      { owner: tx-sender }
      { identity-id: new-id }
    )

    (ok new-id)
  )
)

;; Update identity metadata
(define-public (update-metadata
    (name (optional (string-utf8 100)))
    (birth-year (optional uint))
    (gender (optional (string-ascii 20)))
    (nationality (optional (string-ascii 50)))
    (languages (optional (list 5 (string-ascii 20)))))
  (let
    (
      (identity-id-data (unwrap! (map-get? identity-by-principal { owner: tx-sender }) (err u404)))
      (identity-id (get identity-id identity-id-data))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )

    (map-set identity-metadata
      { identity-id: identity-id }
      {
        name: name,
        birth-year: birth-year,
        gender: gender,
        nationality: nationality,
        languages: languages,
        last-updated: current-time
      }
    )

    (ok true)
  )
)

;; Deactivate an identity
(define-public (deactivate-identity)
  (let
    (
      (identity-id-data (unwrap! (map-get? identity-by-principal { owner: tx-sender }) (err u404)))
      (identity-id (get identity-id identity-id-data))
      (identity (unwrap! (map-get? identities { identity-id: identity-id }) (err u404)))
    )

    (map-set identities
      { identity-id: identity-id }
      (merge identity { status: "INACTIVE" })
    )

    (ok true)
  )
)

;; Reactivate an identity
(define-public (reactivate-identity)
  (let
    (
      (identity-id-data (unwrap! (map-get? identity-by-principal { owner: tx-sender }) (err u404)))
      (identity-id (get identity-id identity-id-data))
      (identity (unwrap! (map-get? identities { identity-id: identity-id }) (err u404)))
    )

    (map-set identities
      { identity-id: identity-id }
      (merge identity { status: "ACTIVE" })
    )

    (ok true)
  )
)

;; Recover an identity (transfer to recovery address)
(define-public (recover-identity (identity-id uint))
  (let
    (
      (identity (unwrap! (map-get? identities { identity-id: identity-id }) (err u404)))
      (recovery-address (unwrap! (get recovery-address identity) (err u2)))
    )
    ;; Only the recovery address can recover
    (asserts! (is-eq tx-sender recovery-address) (err u403))

    ;; Remove old principal mapping
    (map-delete identity-by-principal { owner: (get owner identity) })

    ;; Update identity owner
    (map-set identities
      { identity-id: identity-id }
      (merge identity {
        owner: tx-sender,
        recovery-address: none
      })
    )

    ;; Create new principal mapping
    (map-set identity-by-principal
      { owner: tx-sender }
      { identity-id: identity-id }
    )

    (ok true)
  )
)

;; Update recovery address
(define-public (update-recovery-address (recovery-address (optional principal)))
  (let
    (
      (identity-id-data (unwrap! (map-get? identity-by-principal { owner: tx-sender }) (err u404)))
      (identity-id (get identity-id identity-id-data))
      (identity (unwrap! (map-get? identities { identity-id: identity-id }) (err u404)))
    )

    (map-set identities
      { identity-id: identity-id }
      (merge identity { recovery-address: recovery-address })
    )

    (ok true)
  )
)

;; Get identity by ID
(define-read-only (get-identity (identity-id uint))
  (map-get? identities { identity-id: identity-id })
)

;; Get identity metadata
(define-read-only (get-identity-metadata (identity-id uint))
  (map-get? identity-metadata { identity-id: identity-id })
)

;; Get identity ID by principal
(define-read-only (get-identity-by-principal (owner principal))
  (map-get? identity-by-principal { owner: owner })
)

;; Check if identity exists and is active
(define-read-only (is-identity-active (identity-id uint))
  (match (map-get? identities { identity-id: identity-id })
    identity (is-eq (get status identity) "ACTIVE")
    false
  )
)

;; Check if principal owns an identity
(define-read-only (has-identity (owner principal))
  (is-some (map-get? identity-by-principal { owner: owner }))
)
