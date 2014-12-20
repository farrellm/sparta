(ns sparta.analyzer
  (:require [clojure.string :refer [join]]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.core.match :refer [match]]))

(def infix-ops (into #{} '(+ - * / ^ & | && ||)))

(defn un-interleave [n s]
  (apply map list (partition n s)))

(defn global-env []
  {})

(defn run-passes [ast]
  ast)

(defn -analyze [form env]
  (ana/analyze form env))

(defn *macroexpand-1 [form env]
  (macroexpand-1 form))

(defn create-var [form env]
  form)

(defn analyze [form env]
  (binding [ana/macroexpand-1 *macroexpand-1
            ana/create-var   create-var
            ana/parse         ana/-parse
            ana/var?          var?]
    (env/ensure (global-env)
                (run-passes (-analyze form env)))))

(defmulti -emit-form
  (fn [ast]
    (if-let [type (:type ast)]
      type
      (if-let [op (:op ast)]
        op
        []))))
(defmethod -emit-form :number [ast]
  (str (:val ast)))
(defmethod -emit-form :string [ast]
  (str "'" (:val ast) "'"))
(defmethod -emit-form :keyword [ast]
  (str "'" (:val ast) "'"))
(defmethod -emit-form :maybe-class [ast]
  (str (:class ast)))
(defmethod -emit-form :vector [ast]
  (apply str "list(" (join ", " (mapcat -emit-form (:items ast))) ")"))

(defmethod -emit-form :with-meta [ast]
  (-> ast :expr -emit-form))

(defmethod -emit-form :fn [ast]
  (when-not (= 1 (-> ast :methods count))
    (throw (RuntimeException. "only functions of a single arity supported")))

  (let [method (-> ast :methods first)]
    (str "(function (" (join ", " (map :name (:params method))) ") "
         (-> ast :methods first :body -emit-form)
         ")")))

(defmethod -emit-form :local [ast]
  (str (:name ast)))

(defmethod -emit-form :do [ast]
  (str "{ " (->> (-> ast :statements  (conj (-> ast :ret)))
              (map -emit-form) (join "; ")) " }"))

(defmethod -emit-form :let [ast]
  (str "{ "
       (join "; " (concat (->> ast :bindings (map #(str (:name %) " <- " (-> % :init -emit-form))))
                          [(-> ast :body -emit-form)]))
       " }"))

(defmethod -emit-form :invoke [ast]
  (let [f (-> ast :fn)]
    (if-let [op (and (-> f :op (= :maybe-class)) (-> f )
                     (-> f :class (infix-ops)))]
      (->> ast :args (map -emit-form) (join (str op)))
      (str (-emit-form f)
           "(" (->> ast :args (map -emit-form) (join ", ")) ")"))))

(defmethod -emit-form :def [ast]
  (str (:var ast) " <<- " (-> ast :init -emit-form)))

(defmethod -emit-form :default [ast]
  {:default ast})

(defn emit-form [ast]
  (str (-emit-form ast) (if (:top-level ast)
                          "; " nil)))

(emit-form (analyze '8 {}))
(emit-form (analyze '(do 8 9) {}))
