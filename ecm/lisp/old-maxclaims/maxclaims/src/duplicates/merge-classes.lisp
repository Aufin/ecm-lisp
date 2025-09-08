(in-package :maxclaims)

(define-mergeable-table person
    ((policy :key policy-id :foreign-keys (agent-id insured-id company-id underwriter-id branch-id agency-office-id))
     (contract :key contract-id :foreign-keys (agency-id insurance-company-id))
     (app-user :key  app-user-id :foreign-keys (person-id))
     (claim :key claim-id :foreign-keys (adjuster-id plaintiff-id))
     (claim-transaction :key transaction-id :foreign-keys (payee-id))))

(define-mergeable-table contract
  ((risk :key risk-id :foreign-keys (contract-id))
   (app-user-contract :key app-user-app-resource-id :foreign-keys (contract-id))))

(define-mergeable-table policy
  ((risk :key risk-id :foreign-keys (policy-id))))