;; Digital Hiking Group Coordinator
;; A comprehensive trail group organization system with safety features

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TRIP-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-REGISTERED (err u102))
(define-constant ERR-NOT-REGISTERED (err u103))
(define-constant ERR-TRIP-FULL (err u104))
(define-constant ERR-INVALID-DIFFICULTY (err u105))
(define-constant ERR-TRIP-STARTED (err u106))
(define-constant ERR-TRIP-NOT-STARTED (err u107))
(define-constant ERR-ALREADY-CHECKED-IN (err u108))
(define-constant ERR-CHECK-IN-OVERDUE (err u109))

;; Data variables
(define-data-var trip-counter uint u0)
(define-data-var contract-owner principal tx-sender)

;; Trip difficulty levels (1-5 scale)
(define-constant DIFFICULTY-EASY u1)
(define-constant DIFFICULTY-MODERATE u2)
(define-constant DIFFICULTY-CHALLENGING u3)
(define-constant DIFFICULTY-DIFFICULT u4)
(define-constant DIFFICULTY-EXPERT u5)

;; Trip status constants
(define-constant STATUS-PLANNED u0)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)

;; Data structures
(define-map trips
  uint
  {
    organizer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 200),
    difficulty: uint,
    max-participants: uint,
    start-block: uint,
    duration-blocks: uint,
    status: uint,
    emergency-contact: (string-ascii 100),
    created-at: uint
  })

(define-map trip-participants
  { trip-id: uint, participant: principal }
  {
    registered-at: uint,
    emergency-contact: (string-ascii 100),
    checked-in: bool,
    last-check-in: uint
  })

(define-map trip-participant-count
  uint
  uint)

(define-map user-emergency-contacts
  principal
  (string-ascii 100))

;; Safety check-in requirements (blocks between required check-ins)
(define-map trip-check-in-intervals
  uint
  uint)

;; Public functions

;; Create a new hiking trip
(define-public (create-trip
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 200))
    (difficulty uint)
    (max-participants uint)
    (start-block uint)
    (duration-blocks uint)
    (emergency-contact (string-ascii 100))
    (check-in-interval uint))
  (let
    ((trip-id (+ (var-get trip-counter) u1)))
    (asserts! (and (>= difficulty DIFFICULTY-EASY) (<= difficulty DIFFICULTY-EXPERT)) ERR-INVALID-DIFFICULTY)
    (asserts! (> max-participants u0) ERR-NOT-AUTHORIZED)
    (asserts! (> start-block stacks-block-height) ERR-NOT-AUTHORIZED)
    (asserts! (> duration-blocks u0) ERR-NOT-AUTHORIZED)

    (map-set trips trip-id
      {
        organizer: tx-sender,
        title: title,
        description: description,
        location: location,
        difficulty: difficulty,
        max-participants: max-participants,
        start-block: start-block,
        duration-blocks: duration-blocks,
        status: STATUS-PLANNED,
        emergency-contact: emergency-contact,
        created-at: stacks-block-height
      })

    (map-set trip-participant-count trip-id u0)
    (map-set trip-check-in-intervals trip-id check-in-interval)
    (var-set trip-counter trip-id)

    (ok trip-id)))

;; Register for a trip
(define-public (register-for-trip (trip-id uint) (emergency-contact (string-ascii 100)))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND))
     (current-count (default-to u0 (map-get? trip-participant-count trip-id))))

    (asserts! (is-none (map-get? trip-participants { trip-id: trip-id, participant: tx-sender })) ERR-ALREADY-REGISTERED)
    (asserts! (< current-count (get max-participants trip-data)) ERR-TRIP-FULL)
    (asserts! (is-eq (get status trip-data) STATUS-PLANNED) ERR-TRIP-STARTED)

    (map-set trip-participants
      { trip-id: trip-id, participant: tx-sender }
      {
        registered-at: stacks-block-height,
        emergency-contact: emergency-contact,
        checked-in: false,
        last-check-in: u0
      })

    (map-set trip-participant-count trip-id (+ current-count u1))
    (map-set user-emergency-contacts tx-sender emergency-contact)

    (ok true)))

;; Start a trip (only organizer can do this)
(define-public (start-trip (trip-id uint))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND)))

    (asserts! (is-eq (get organizer trip-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status trip-data) STATUS-PLANNED) ERR-TRIP-STARTED)
    (asserts! (>= stacks-block-height (get start-block trip-data)) ERR-NOT-AUTHORIZED)

    (map-set trips trip-id
      (merge trip-data { status: STATUS-ACTIVE }))

    (ok true)))

;; Safety check-in for participants
(define-public (check-in (trip-id uint))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND))
     (participant-data (unwrap! (map-get? trip-participants { trip-id: trip-id, participant: tx-sender }) ERR-NOT-REGISTERED)))

    (asserts! (is-eq (get status trip-data) STATUS-ACTIVE) ERR-TRIP-NOT-STARTED)

    (map-set trip-participants
      { trip-id: trip-id, participant: tx-sender }
      (merge participant-data
        {
          checked-in: true,
          last-check-in: stacks-block-height
        }))

    (ok true)))

;; Complete a trip (only organizer)
(define-public (complete-trip (trip-id uint))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND)))

    (asserts! (is-eq (get organizer trip-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status trip-data) STATUS-ACTIVE) ERR-TRIP-NOT-STARTED)

    (map-set trips trip-id
      (merge trip-data { status: STATUS-COMPLETED }))

    (ok true)))

;; Cancel a trip (only organizer)
(define-public (cancel-trip (trip-id uint))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND)))

    (asserts! (is-eq (get organizer trip-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status trip-data) STATUS-COMPLETED)) ERR-NOT-AUTHORIZED)

    (map-set trips trip-id
      (merge trip-data { status: STATUS-CANCELLED }))

    (ok true)))

;; Update emergency contact
(define-public (update-emergency-contact (contact (string-ascii 100)))
  (begin
    (map-set user-emergency-contacts tx-sender contact)
    (ok true)))

;; Emergency function to mark participant as overdue (only organizer or contract owner)
(define-public (mark-participant-overdue (trip-id uint) (participant principal))
  (let
    ((trip-data (unwrap! (map-get? trips trip-id) ERR-TRIP-NOT-FOUND))
     (participant-data (unwrap! (map-get? trip-participants { trip-id: trip-id, participant: participant }) ERR-NOT-REGISTERED))
     (check-in-interval (default-to u144 (map-get? trip-check-in-intervals trip-id))))

    (asserts!
      (or
        (is-eq (get organizer trip-data) tx-sender)
        (is-eq (var-get contract-owner) tx-sender))
      ERR-NOT-AUTHORIZED)

    (asserts! (is-eq (get status trip-data) STATUS-ACTIVE) ERR-TRIP-NOT-STARTED)
    (asserts!
      (> (- stacks-block-height (get last-check-in participant-data)) check-in-interval)
      ERR-NOT-AUTHORIZED)

    ;; This would trigger emergency protocols in a real implementation
    (ok true)))

;; Read-only functions

;; Get trip details
(define-read-only (get-trip (trip-id uint))
  (map-get? trips trip-id))

;; Get participant data
(define-read-only (get-participant-data (trip-id uint) (participant principal))
  (map-get? trip-participants { trip-id: trip-id, participant: participant }))

;; Get trip participant count
(define-read-only (get-participant-count (trip-id uint))
  (default-to u0 (map-get? trip-participant-count trip-id)))

;; Check if participant is overdue for check-in
(define-read-only (is-participant-overdue (trip-id uint) (participant principal))
  (match (map-get? trip-participants { trip-id: trip-id, participant: participant })
    participant-data
    (let
      ((check-in-interval (default-to u144 (map-get? trip-check-in-intervals trip-id)))
       (last-check (get last-check-in participant-data)))
      (and
        (> last-check u0)
        (> (- stacks-block-height last-check) check-in-interval)))
    false))

;; Get user's emergency contact
(define-read-only (get-emergency-contact (user principal))
  (map-get? user-emergency-contacts user))

;; Get difficulty rating description
(define-read-only (get-difficulty-description (difficulty uint))
  (if (is-eq difficulty DIFFICULTY-EASY) "Easy - Suitable for beginners"
    (if (is-eq difficulty DIFFICULTY-MODERATE) "Moderate - Some hiking experience recommended"
      (if (is-eq difficulty DIFFICULTY-CHALLENGING) "Challenging - Good fitness level required"
        (if (is-eq difficulty DIFFICULTY-DIFFICULT) "Difficult - Advanced hikers only"
          (if (is-eq difficulty DIFFICULTY-EXPERT) "Expert - Extreme conditions, technical skills required"
            "Invalid difficulty level"))))))

;; Get trip status description
(define-read-only (get-status-description (status uint))
  (if (is-eq status STATUS-PLANNED) "Planned"
    (if (is-eq status STATUS-ACTIVE) "Active"
      (if (is-eq status STATUS-COMPLETED) "Completed"
        (if (is-eq status STATUS-CANCELLED) "Cancelled"
          "Unknown")))))

;; Check if trip registration is open
(define-read-only (is-registration-open (trip-id uint))
  (match (map-get? trips trip-id)
    trip-data
    (and
      (is-eq (get status trip-data) STATUS-PLANNED)
      (< (default-to u0 (map-get? trip-participant-count trip-id)) (get max-participants trip-data))
      (< stacks-block-height (get start-block trip-data)))
    false))

;; Get total number of trips created
(define-read-only (get-total-trips)
  (var-get trip-counter))

;; Administrative functions

;; Transfer contract ownership (only current owner)
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)))

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner))
