(ns leap)

(defn leap-year? [year] ;; <- argslist goes here
  (cond (zero? (mod year 400)) true
        (zero? (mod year 100)) false
        (zero? (mod year 4)) true
        :default false)
)
