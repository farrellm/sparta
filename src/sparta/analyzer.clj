(ns sparta.analyzer
  (:require [clojure.string :refer [join]]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.core.match :refer [match]]))

(defn global-env []
  {})

(defn run-passes [ast]
  ast)

(defn -analyze [form env]
  (ana/analyze form env))

(defn *macroexpand-1 [form env]
  (cond
    (seq form)
    (let [[op & args] form]
      (match op
        'let* (let [[_ bindings & exprs] form
                    [names vals] (partition 2 bindings)]
                `((fn [~@names] ~@vals)))
        _     (macroexpand-1 form)))
    
    :else
    (macroexpand-1 form))
  )

(defn analyze [form env]
  (binding [ana/macroexpand-1 *macroexpand-1
            #_(ana/create-var    create-var)
            ana/parse         ana/-parse
            ana/var?          var?]
    (env/ensure (global-env)
                (run-passes (-analyze form env)))))

(def emit-form nil)
(defmulti emit-form
  (fn [ast]
    (if-let [type (:type ast)]
      type
      (if-let [op (:op ast)]
        op
        []))))
(defmethod emit-form :number [ast]
  (str (:val ast)))
(defmethod emit-form :string [ast]
  (str "'" (:val ast) "'"))
(defmethod emit-form :keyword [ast]
  (str ("'" (:val ast) "'")))
(defmethod emit-form :maybe-class [ast]
  (str (:class ast)))
(defmethod emit-form :vector [ast]
  (apply str "list(" (join ", " (mapcat emit-form (:items ast))) ")"))

(defmethod emit-form :with-meta [ast]
  (-> ast :expr emit-form))

(defmethod emit-form :fn [ast]
  (when-not (= 1 (-> ast :methods count))
    (throw (RuntimeException. "only functions of a single arity supported")))
  (let [method (-> ast :methods (get 0))]
    (str "(function (" (join ", " (map :name (:params method)))
         ") { "
         (join "; "
               (concat (->> method :body :statements (map emit-form))
                       [(str "return( " (-> method :body :ret emit-form) " )")]))
         " })")))

(defmethod emit-form :local [ast]
  (str (:name ast)))

(defmethod emit-form :default [ast]
  {:default ast})


(emit-form (analyze '8 {}))
(emit-form (analyze '8.8 {}))
(emit-form (analyze "8" {}))
(emit-form (analyze 'a {}))
(emit-form (analyze ':a {}))
(emit-form (analyze '[a b] {}))
(analyze '(do 1 2 3) {})
(-> (analyze '(fn [x] 1 x) {}) :expr :methods (get 0) :body :ret)
(emit-form (analyze '(fn [x y] 1 2 x) {}))
(analyze '(let [x 0] x) {})

(analyze '(if a b c) {})
(analyze '(if-not a b c) {})
(analyze '(when a b) {})

(analyze '(def a 8) {})

(macroexpand-1 '(fn [x] 1 x))

(macroexpand-1 '(if-not a b c))

(macroexpand-1 '(a. b c))
(analyze '(. a b c) {})
(analyze '(a b c) {})
