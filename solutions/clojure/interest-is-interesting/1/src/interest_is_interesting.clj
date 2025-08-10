(ns interest-is-interesting)

(defn interest-rate
  "Given a BigDecimal balance, return the related interest rate"
  [balance]
  (cond
    (< balance 0) -3.213
    (< balance 1000) 0.5
    (< balance 5000) 1.621
    (>= balance 5000) 2.475
  ))

(defn annual-balance-update
  "Given a BigDecimal balance, get interest-rate and calc/return new balance"
  [balance]
  (+ (* (bigdec (interest-rate balance)) 0.01M (abs balance)) balance))

(defn amount-to-donate
  "Given a BigDecimal balance and tax free precentage calu/return donation amt"
  [balance tax-free-percentage]
  (cond
    (<= balance 0) 0
    (> balance 0) (int (* tax-free-percentage 0.01 balance 2))
    ))