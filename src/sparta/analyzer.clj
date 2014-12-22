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
  (match [form]
    [(['ns n & _] :seq)] form
    :else (macroexpand-1 form)))

(defn create-var [form env]
  form)

(defn analyze
  ([form] (analyze form {}))
  ([form env]
   (binding [ana/macroexpand-1 *macroexpand-1
             ana/create-var    create-var
             ana/parse         ana/-parse
             ana/var?          var?]
     (env/ensure (global-env)
                 (run-passes (-analyze form env))))))

(def -emit-form nil)
(defmulti -emit-form
  (fn [ast set-ns]
    (if-let [type (:type ast)]
      type
      (if-let [op (:op ast)]
        op
        []))))
(defmethod -emit-form :number [ast set-ns]
  (str (:val ast)))
(defmethod -emit-form :string [ast set-ns]
  (str "'" (:val ast) "'"))
(defmethod -emit-form :keyword [ast set-ns]
  (str "'" (:val ast) "'"))
(defmethod -emit-form :maybe-class [ast set-ns]
  (str (:class ast)))
(defmethod -emit-form :vector [ast set-ns]
  (apply str "list(" (join ", " (mapcat #(-emit-form % set-ns) (:items ast))) ")"))

(defmethod -emit-form :with-meta [ast set-ns]
  (-> ast :expr (-emit-form set-ns)))

(defmethod -emit-form :fn [ast set-ns]
  (when-not (= 1 (-> ast :methods count))
    (throw (RuntimeException. "only functions of a single arity supported")))

  (let [method (-> ast :methods first)
        f (str "function (" (join ", " (map :name (:params method))) ") "
               (-> ast :methods first :body (-emit-form false)))]
    (if set-ns
      (str "{ f <- " f "; environment(f) <- .ns; f }"))))

(defmethod -emit-form :local [ast set-ns]
  (str (:name ast)))

(defmethod -emit-form :do [ast set-ns]
  (str "{ " (->> (-> ast :statements  (conj (-> ast :ret)))
              (map #(-emit-form % set-ns)) (join "; ")) " }"))

(defmethod -emit-form :let [ast set-ns]
  (str "{ "
       (join "; " (concat (->> ast :bindings
                               (map #(str (:name %) " <- " (-> % :init (-emit-form set-ns)))))
                          [(-> ast :body (-emit-form set-ns))]))
       " }"))

(defmethod -emit-form :invoke [ast set-ns]
  (let [f (-> ast :fn)]
    (if-let [op (and (-> f :op (= :maybe-class)) (-> f )
                     (-> f :class (infix-ops)))]
      (->> ast :args (map #(-emit-form % set-ns)) (join (str op)))
      (match f
        {:class 'ns} #_(str "assign('.ns', as.environment('"
                          (-> ast :args first :class)
                          "'), envir = .GlobalEnv)")
        (let [n (-> ast :args first :class)]
          (str "{ if (!exists('" n "', envir=.GlobalEnv)) "
               "assign('" n "', new.env(parent=emptyenv()), envir=.GlobalEnv); "
               "assign('.ns', .GlobalEnv[['" n "']], envir=.GlobalEnv)"
               " }"))
        :else (str (-emit-form f set-ns)
                   "(" (->> ast :args (map #(-emit-form % set-ns)) (join ", ")) ")")))))

(defmethod -emit-form :def [ast set-ns]
  (str "assign('" (-> ast :var str (.replaceAll "-" "."))
       "', " (-> ast :init (-emit-form set-ns)) ", envir = .GlobalEnv$.ns)"))

(defmethod -emit-form :host-call [ast set-ns]
  (if (= '- (-> ast :target :class))
    (str (:method ast) "("
         (->> ast :args (map #(-emit-form % set-ns)) (join ", ")) ")")
    (str "as.environment('" (-> ast :target :class) "')$"
         (:method ast) "("
         (->> ast :args (map #(-emit-form % set-ns)) (join ", ")) ")")))

(defmethod -emit-form :if [ast set-ns]
  (let [e (-> ast :else )]
    (str "if (" (-> ast :test (-emit-form set-ns)) ") "
         (-> ast :then (-emit-form set-ns))
         (if-not (and (= (:op e):const) (= (:val e) nil))
           (str " else " (-> ast :else (-emit-form set-ns)))))))

(defmethod -emit-form :default [ast set-ns]
  [ast set-ns])

(defn emit-form [ast]
  (str (-emit-form ast (:top-level ast))
       (if (:top-level ast) "; " nil)))
