;; Autonomous Multi-Stakeholder Recall Orchestration Platform
;; A decentralized system for managing product recalls across supply chains

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-status (err u104))
(define-constant err-insufficient-risk (err u105))

;; Data Variables
(define-data-var recall-nonce uint u0)
(define-data-var stakeholder-nonce uint u0)

;; Stakeholder roles
(define-constant role-manufacturer u1)
(define-constant role-distributor u2)
(define-constant role-retailer u3)
(define-constant role-regulator u4)

;; Recall status
(define-constant status-detected u1)
(define-constant status-initiated u2)
(define-constant status-active u3)
(define-constant status-completed u4)
(define-constant status-closed u5)

;; Data Maps
(define-map stakeholders
  { stakeholder-id: uint }
  {
    address: principal,
    role: uint,
    name: (string-ascii 50),
    active: bool,
    registered-at: uint
  }
)

(define-map stakeholder-by-address
  { address: principal }
  { stakeholder-id: uint }
)

(define-map recalls
  { recall-id: uint }
  {
    product-id: (string-ascii 100),
    batch-number: (string-ascii 50),
    initiator: uint,
    risk-score: uint,
    status: uint,
    affected-quantity: uint,
    reason: (string-ascii 200),
    initiated-at: uint,
    completed-at: (optional uint)
  }
)

(define-map recall-impacts
  { recall-id: uint, stakeholder-id: uint }
  {
    financial-impact: uint,
    health-risk: uint,
    affected-units: uint,
    acknowledged: bool,
    responded-at: (optional uint)
  }
)

(define-map compliance-records
  { recall-id: uint, stakeholder-id: uint }
  {
    compliant: bool,
    verified-at: uint,
    verifier: principal,
    compliance-hash: (buff 32)
  }
)

(define-map iot-sensors
  { sensor-id: (string-ascii 50) }
  {
    stakeholder-id: uint,
    sensor-type: (string-ascii 30),
    location: (string-ascii 100),
    active: bool,
    last-reading: uint
  }
)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-valid-stakeholder (stakeholder-id uint))
  (match (map-get? stakeholders { stakeholder-id: stakeholder-id })
    stakeholder (get active stakeholder)
    false
  )
)

(define-private (is-regulator (stakeholder-id uint))
  (match (map-get? stakeholders { stakeholder-id: stakeholder-id })
    stakeholder (is-eq (get role stakeholder) role-regulator)
    false
  )
)

;; Public functions - Stakeholder Management
(define-public (register-stakeholder (address principal) (role uint) (name (string-ascii 50)))
  (let
    (
      (new-id (+ (var-get stakeholder-nonce) u1))
    )
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (is-none (map-get? stakeholder-by-address { address: address })) err-already-exists)
    
    (map-set stakeholders
      { stakeholder-id: new-id }
      {
        address: address,
        role: role,
        name: name,
        active: true,
        registered-at: block-height
      }
    )
    
    (map-set stakeholder-by-address
      { address: address }
      { stakeholder-id: new-id }
    )
    
    (var-set stakeholder-nonce new-id)
    (ok new-id)
  )
)

(define-public (deactivate-stakeholder (stakeholder-id uint))
  (let
    (
      (stakeholder (unwrap! (map-get? stakeholders { stakeholder-id: stakeholder-id }) err-not-found))
    )
    (asserts! (is-contract-owner) err-owner-only)
    
    (map-set stakeholders
      { stakeholder-id: stakeholder-id }
      (merge stakeholder { active: false })
    )
    (ok true)
  )
)

;; Public functions - Recall Management
(define-public (initiate-recall 
  (product-id (string-ascii 100))
  (batch-number (string-ascii 50))
  (risk-score uint)
  (affected-quantity uint)
  (reason (string-ascii 200))
)
  (let
    (
      (new-id (+ (var-get recall-nonce) u1))
      (stakeholder-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (stakeholder-id (get stakeholder-id stakeholder-data))
    )
    (asserts! (is-valid-stakeholder stakeholder-id) err-unauthorized)
    (asserts! (>= risk-score u50) err-insufficient-risk)
    
    (map-set recalls
      { recall-id: new-id }
      {
        product-id: product-id,
        batch-number: batch-number,
        initiator: stakeholder-id,
        risk-score: risk-score,
        status: status-initiated,
        affected-quantity: affected-quantity,
        reason: reason,
        initiated-at: block-height,
        completed-at: none
      }
    )
    
    (var-set recall-nonce new-id)
    (ok new-id)
  )
)

(define-public (update-recall-status (recall-id uint) (new-status uint))
  (let
    (
      (recall (unwrap! (map-get? recalls { recall-id: recall-id }) err-not-found))
      (stakeholder-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (stakeholder-id (get stakeholder-id stakeholder-data))
    )
    (asserts! (or 
      (is-eq stakeholder-id (get initiator recall))
      (is-regulator stakeholder-id)
    ) err-unauthorized)
    (asserts! (<= new-status status-closed) err-invalid-status)
    
    (map-set recalls
      { recall-id: recall-id }
      (merge recall { 
        status: new-status,
        completed-at: (if (is-eq new-status status-closed) (some block-height) (get completed-at recall))
      })
    )
    (ok true)
  )
)

(define-public (record-impact 
  (recall-id uint)
  (stakeholder-id uint)
  (financial-impact uint)
  (health-risk uint)
  (affected-units uint)
)
  (let
    (
      (sender-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (sender-id (get stakeholder-id sender-data))
      (recall (unwrap! (map-get? recalls { recall-id: recall-id }) err-not-found))
    )
    (asserts! (is-eq sender-id stakeholder-id) err-unauthorized)
    
    (map-set recall-impacts
      { recall-id: recall-id, stakeholder-id: stakeholder-id }
      {
        financial-impact: financial-impact,
        health-risk: health-risk,
        affected-units: affected-units,
        acknowledged: true,
        responded-at: (some block-height)
      }
    )
    (ok true)
  )
)

(define-public (verify-compliance 
  (recall-id uint)
  (stakeholder-id uint)
  (compliant bool)
  (compliance-hash (buff 32))
)
  (let
    (
      (verifier-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (verifier-id (get stakeholder-id verifier-data))
      (recall (unwrap! (map-get? recalls { recall-id: recall-id }) err-not-found))
    )
    (asserts! (is-regulator verifier-id) err-unauthorized)
    
    (map-set compliance-records
      { recall-id: recall-id, stakeholder-id: stakeholder-id }
      {
        compliant: compliant,
        verified-at: block-height,
        verifier: tx-sender,
        compliance-hash: compliance-hash
      }
    )
    (ok true)
  )
)

;; Public functions - IoT Sensor Management
(define-public (register-sensor 
  (sensor-id (string-ascii 50))
  (sensor-type (string-ascii 30))
  (location (string-ascii 100))
)
  (let
    (
      (stakeholder-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (stakeholder-id (get stakeholder-id stakeholder-data))
    )
    (asserts! (is-valid-stakeholder stakeholder-id) err-unauthorized)
    
    (map-set iot-sensors
      { sensor-id: sensor-id }
      {
        stakeholder-id: stakeholder-id,
        sensor-type: sensor-type,
        location: location,
        active: true,
        last-reading: block-height
      }
    )
    (ok true)
  )
)

(define-public (update-sensor-reading (sensor-id (string-ascii 50)))
  (let
    (
      (sensor (unwrap! (map-get? iot-sensors { sensor-id: sensor-id }) err-not-found))
      (stakeholder-data (unwrap! (map-get? stakeholder-by-address { address: tx-sender }) err-unauthorized))
      (stakeholder-id (get stakeholder-id stakeholder-data))
    )
    (asserts! (is-eq stakeholder-id (get stakeholder-id sensor)) err-unauthorized)
    
    (map-set iot-sensors
      { sensor-id: sensor-id }
      (merge sensor { last-reading: block-height })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-stakeholder (stakeholder-id uint))
  (map-get? stakeholders { stakeholder-id: stakeholder-id })
)

(define-read-only (get-stakeholder-by-address (address principal))
  (match (map-get? stakeholder-by-address { address: address })
    data (map-get? stakeholders { stakeholder-id: (get stakeholder-id data) })
    none
  )
)

(define-read-only (get-recall (recall-id uint))
  (map-get? recalls { recall-id: recall-id })
)

(define-read-only (get-recall-impact (recall-id uint) (stakeholder-id uint))
  (map-get? recall-impacts { recall-id: recall-id, stakeholder-id: stakeholder-id })
)

(define-read-only (get-compliance-record (recall-id uint) (stakeholder-id uint))
  (map-get? compliance-records { recall-id: recall-id, stakeholder-id: stakeholder-id })
)

(define-read-only (get-sensor (sensor-id (string-ascii 50)))
  (map-get? iot-sensors { sensor-id: sensor-id })
)

(define-read-only (get-total-recalls)
  (ok (var-get recall-nonce))
)

(define-read-only (get-total-stakeholders)
  (ok (var-get stakeholder-nonce))
)