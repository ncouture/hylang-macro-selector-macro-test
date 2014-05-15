(import [kwzip [group-map keyword? one]])


(defmacro define [variables &rest body]
  "Kinda like let"
  (import [hy [HyList]])
  (setv macroed_variables [])
  (if (not (isinstance variables HyList))
    (macro-error variables "define lexical context must be a list"))
  (for* [variable variables]
    (if (isinstance variable HyList)
      (do (if (!= (len variable) 2)
            (macro-error variable "define variable assignments must contain two items"))
            (.append macroed-variables `(setv ~(get variable 0) ~(get variable 1))))
      (.append macroed-variables `(setv ~variable None))))
  `(do
     ~@macroed-variables
     ~@body))


(defmacro which [&rest forms]
  (define [[data (group-map keyword? forms)]
           [type (one `nil (:type data))]
           [uno (one `nil (:uno data))]
           [dos (one `nil (:dos data))]]
    `(do
      (if (= ~type "uno")
        (uno :uno ~uno))
      (if (= ~type "dos")
        (dos :dos ~dos)))))


(defmacro uno [&rest forms]
  (define [[data (group-map keyword? forms)]
           [type (one `nil (:type data))]
           [uno (one `nil (:uno data))]]
    `((fn [] (print ~uno)))))


(defmacro dos [&rest forms]
  (define [[data (group-map keyword? forms)]
           [type (one `nil (:type data))]
           [dos (one `nil (:dos data))]]
    `((fn [] (print ~dos)))))
