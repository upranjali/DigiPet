;; Digital Pet NFT Contract
;; NFT pets that evolve and grow based on user interactions

;; Define the NFT
(define-non-fungible-token digital-pet uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-pet-owner (err u101))
(define-constant err-pet-not-found (err u102))
(define-constant err-invalid-pet-id (err u103))
(define-constant err-max-level-reached (err u104))
(define-constant err-invalid-name (err u105))

;; Pet evolution stages
(define-constant stage-egg u0)
(define-constant stage-baby u1)
(define-constant stage-adult u2)
(define-constant stage-legendary u3)

;; Pet data structure
(define-map pet-data uint {
  owner: principal,
  name: (string-ascii 50),
  stage: uint,
  experience: uint,
  last-interaction: uint,
  happiness: uint,
  created-at: uint
})

;; Track next pet ID
(define-data-var next-pet-id uint u1)

;; Track total pets minted
(define-data-var total-pets uint u0)

;; Function 1: Mint a new digital pet (starts as egg)
(define-public (mint-pet (pet-name (string-ascii 50)))
  (let ((pet-id (var-get next-pet-id))
        (current-time stacks-block-height))
    ;; Validate pet name is not empty
    (asserts! (> (len pet-name) u0) err-invalid-name)
    (try! (nft-mint? digital-pet pet-id tx-sender))
    (map-set pet-data pet-id {
      owner: tx-sender,
      name: pet-name,
      stage: stage-egg,
      experience: u0,
      last-interaction: current-time,
      happiness: u50,
      created-at: current-time
    })
    (var-set next-pet-id (+ pet-id u1))
    (var-set total-pets (+ (var-get total-pets) u1))
    (print {action: "pet-minted", pet-id: pet-id, owner: tx-sender, name: pet-name})
    (ok pet-id)))

;; Function 2: Interact with pet (feed, play, care) - increases experience and potentially evolves
(define-public (interact-with-pet (pet-id uint) (interaction-type (string-ascii 20)))
  (let ((pet-info (unwrap! (map-get? pet-data pet-id) err-pet-not-found))
        (current-time stacks-block-height))
    ;; Verify ownership
    (asserts! (is-eq (nft-get-owner? digital-pet pet-id) (some tx-sender)) err-not-pet-owner)
    
    (let ((current-experience (get experience pet-info))
          (current-stage (get stage pet-info))
          (current-happiness (get happiness pet-info))
          (experience-gain u10)
          (happiness-gain u5)
          (new-experience (+ current-experience experience-gain))
          (new-happiness (if (< (+ current-happiness happiness-gain) u100)
                           (+ current-happiness happiness-gain)
                           u100))
          ;; Determine new stage based on experience
          (new-stage (if (and (is-eq current-stage stage-egg) (>= new-experience u30))
                       stage-baby
                       (if (and (is-eq current-stage stage-baby) (>= new-experience u100))
                         stage-adult
                         (if (and (is-eq current-stage stage-adult) (>= new-experience u300) (>= new-happiness u90))
                           stage-legendary
                           current-stage)))))
      
      ;; Update pet data
      (map-set pet-data pet-id {
        owner: (get owner pet-info),
        name: (get name pet-info),
        stage: new-stage,
        experience: new-experience,
        last-interaction: current-time,
        happiness: new-happiness,
        created-at: (get created-at pet-info)
      })
      
      ;; Print interaction event
      (print {
        action: "pet-interaction",
        pet-id: pet-id,
        interaction-type: interaction-type,
        new-experience: new-experience,
        new-stage: new-stage,
        new-happiness: new-happiness,
        evolved: (not (is-eq current-stage new-stage))
      })
      
      (ok {
        experience: new-experience,
        stage: new-stage,
        happiness: new-happiness,
        evolved: (not (is-eq current-stage new-stage))
      }))))

;; Read-only functions for getting pet information
(define-read-only (get-pet-data (pet-id uint))
  (map-get? pet-data pet-id))

(define-read-only (get-pet-owner (pet-id uint))
  (nft-get-owner? digital-pet pet-id))

(define-read-only (get-total-pets)
  (var-get total-pets))

(define-read-only (get-next-pet-id)
  (var-get next-pet-id))

;; Get stage name for display
(define-read-only (get-stage-name (stage uint))
  (if (is-eq stage stage-egg)
    "Egg"
    (if (is-eq stage stage-baby)
      "Baby"
      (if (is-eq stage stage-adult)
        "Adult"
        (if (is-eq stage stage-legendary)
          "Legendary"
          "Unknown")))))

;; Check if pet needs attention (hasn't been interacted with recently)
(define-read-only (pet-needs-attention (pet-id uint))
  (match (map-get? pet-data pet-id)
    pet-info (let ((time-since-interaction (- stacks-block-height (get last-interaction pet-info))))
               (> time-since-interaction u100))
    false))