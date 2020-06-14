(ns precog.core
  (:require [clojure.set :refer :all]))

(def ^:dynamic *env*)


(defmulti
  format-problem-message
  "Handles formatting of precondition failure messages."
  (fn [key _] key)
  :default :no-message-formatter)

(defmethod format-problem-message :no-message-formatter [key data]
  (format "Precondition failed: %s; Arguments: %s" key data))

(defmulti get-symbols
  "Get the symbols that are checked"
  (fn [form] (nth form 0))
  :default :default-symbol-finder)

(defmethod get-symbols :default-symbol-finder
  [form] (into #{} (filter symbol? (next form))))

(defn format-problem-messages
  [problems]
  (clojure.string/join "; " (map (fn [[key value]] (format-problem-message key value)) problems)))

(defn do-preconditions
  ([first results]
   (if first
     ()
     `(when (not-empty ~results)
       (throw (ex-info (format-problem-messages (dissoc ~results :bad-arguments)) ~results)))))
  ([first results arg & args]
   (if first
     `(let [symbols# '~(get-symbols arg)]
        (if ~arg
          ~(apply do-preconditions first results args)
          (let [~results (merge-with union ~results {:bad-arguments symbols#, '~(nth arg 0) symbols#})]
            ~(apply do-preconditions false results args))))
     `(let [symbols# '~(get-symbols arg)]
        (if (not-empty (intersection symbols# (:bad-arguments ~results)))
          ~(apply do-preconditions false results args)
          (if ~arg
            ~(apply do-preconditions first results args)
            (let [~results (merge-with union ~results {:bad-arguments symbols#, '~(nth arg 0) symbols#})]
              ~(apply do-preconditions false results args))))))
   )
  )

(defmacro preconditions
  [& args]
  (let [results (gensym "results")]
    `(let [~results {}]
      ~(apply do-preconditions true results args))))

(defmethod format-problem-message 'some? [_ data]
  (format "Must not be nil: %s" (clojure.string/join ", " (sort data))))

(defmethod format-problem-message 'number? [_ data]
  (format "Must be a number: %s" (clojure.string/join ", " (sort data))))

(defmethod format-problem-message 'string? [_ data]
  (format "Must be a string: %s" (clojure.string/join ", " (sort data))))

