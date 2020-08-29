(ns adxlib.core
  (:require [clojure.spec.alpha :as s])
  (:use dataorigin.core)
  (:import [java.util Locale Random]
           [java.time LocalDate LocalTime]
           [java.time.format DateTimeFormatter]
           [clojure.lang ExceptionInfo])
  (:gen-class))
;; TODO: Add origins to namespace, ordering, etc, which contains the name of it, etc.

(s/def ::data-string? #(string? (unwrap %)))
(s/def ::data-integer? #(integer? (unwrap %)))
(s/def ::data-number? #(number? (unwrap %)))
(s/def ::data-positive? #(<= 0 (unwrap %)))
(s/def ::data-boolean? #(boolean? (unwrap %)))
(s/def ::data-date? #(isa? (class (unwrap %)) LocalDate))
(s/def ::data-time? #(isa? (class (unwrap %)) LocalTime))

(s/def ::adx-write-to (s/keys :req [::str ::containers ::indent]))
(s/def ::adx-type (s/or :keyword keyword? :string string? :vector vector? :list list? :seq seq?))
(s/def ::adx-ordering coll?)
(s/def ::adx-standalone-ordering (s/and ::adx-ordering #(not (every? vector? %))))
(s/def ::adx-container map?)
(s/def ::adx-list coll?)
(s/def ::adx-namespace map?)

(defn new-adx-write-to []
  {::str (StringBuilder.), ::containers (),
   ::indent 0})

(defn adx-write [write-to, object]
  {:pre [(s/valid? ::adx-write-to write-to)]
   :post [(s/valid? ::adx-write-to %)]}
  (.append (::str write-to) (str object))
  write-to)
(defn adx-write-container-start [write-to, container-name]
  {:pre [(s/valid? ::adx-write-to write-to)
         (s/valid? string? container-name)]
   :post [(s/valid? ::adx-write-to %)]}
  (let [written
        (adx-write
         write-to
         (str \{ (count (::containers write-to)) \_ container-name))
        container-added (update written ::containers
                               #(cons container-name %))]
    container-added))
(defn adx-write-container-end [write-to]
  {:pre [(s/valid? ::adx-write-to write-to)]
   :post [(s/valid? ::adx-write-to %)]}
  (let [container-name (first (::containers write-to))
        container-removed (update write-to ::containers rest)
        written
        (adx-write
         container-removed
         (str (count (::containers container-removed)) \_ container-name \}))]
    written))
(defn adx-write-separator [write-to]
  (adx-write write-to \;))
(defn adx-write-space [write-to]
  (adx-write write-to \ ))
(defn adx-write-separator-space [write-to]
  (adx-write-space (adx-write-separator write-to)))
(defn adx-write-new-line [write-to]
  (adx-write write-to (System/lineSeparator)))
(defn adx-write-indent
  ([write-to]
   (adx-write-indent write-to 3))
  ([write-to indent-depth]
   (adx-write write-to (apply str (take (* (::indent write-to) indent-depth)
                                        (repeat \ ))))))
(defn adx-write-new-line-indent [write-to]
  (adx-write-indent (adx-write-new-line write-to)))
(defn adx-write-decrease-indent [write-to]
  (update write-to ::indent dec))
(defn adx-write-increase-indent [write-to]
  (update write-to ::indent inc))
(defn adx-write-finish [write-to]
  (str (::str write-to)))

(defn merge-errors [ex-error]
  ;; TODO: Merge ex-error and all its causes into one coherent object.
  ;; This means I'll have to standardize my ex-error system.
  ;; I should probably also use :: for the keys, in case some clojure code I use
  ;; throws ex-errors.
  ex-error
  )

(defn adx-write-with [namespace, write-to, object]
  (adx-write write-to object))

(defn adx-write-with-string [namespace, write-to, object]
  (if-not (s/valid? ::data-string? object)
    (throw (ex-info "Not a string" {:data object :kind ::not-string})))
  (adx-write write-to (unwrap object)))
(defn adx-write-with-int [namespace, write-to, object]
  (if-not (s/valid? ::data-integer? object)
    (throw (ex-info "Not an int" {:data object :kind ::not-integer})))
  (adx-write write-to (unwrap object)))
(defn adx-write-with-positive-int [namespace, write-to, object]
  (if-not (s/valid? ::data-integer? object)
    (throw (ex-info "Not an int" {:data object :kind ::not-integer})))
  (if-not (s/valid? ::data-positive? object)
    (throw (ex-info "Number is negative" {:data object :kind ::not-positive})))
  (adx-write write-to (unwrap object)))
(defn adx-write-with-float [decimals, namespace, write-to, object]
  (if-not (s/valid? ::data-number? object)
    (throw (ex-info "Not a number" {:data object :kind ::not-number})))
  (adx-write write-to
             (String/format Locale/ENGLISH
                            (str "%." decimals "f")
                            (to-array [(double (unwrap object))]))))
(defn adx-write-with-positive-float [decimals, namespace, write-to, object]
  (if-not (s/valid? ::data-number? object)
    (throw (ex-info "Not a number" {:data object :kind ::not-number})))
  (if-not (s/valid? ::data-positive? object)
    (throw (ex-info "Not positive" {:data object :kind ::not-positive})))
  (adx-write-with-float decimals, namespace, write-to, object))
(defn adx-write-with-boolean [namespace, write-to, object]
  (if-not (s/valid? ::data-boolean? object)
    (throw (ex-info "Not a boolean" {:data object :kind ::not-boolean})))
  (adx-write write-to (if (unwrap object) \Y \N)))
(defn adx-write-with-boolean-numeric [namespace, write-to, object]
  (if-not (s/valid? ::data-boolean? object)
    (throw (ex-info "Not a boolean" {:data object :kind ::not-boolean})))
  (adx-write write-to (if (unwrap object) 1 0)))
(defn adx-write-with-maybe [f namespace write-to value]
  (if-not (nil? (unwrap value))
    (f namespace write-to value)
    write-to))
(defn adx-write-with-date [namespace write-to date]
  (if-not (s/valid? ::data-date? date)
    (throw (ex-info "Not a date" {:data date :kind ::not-date})))
  (adx-write write-to (.format (unwrap date)
                               (DateTimeFormatter/BASIC_ISO_DATE))))
(defn adx-write-with-time [namespace write-to time]
  (if-not (s/valid? ::data-time? time)
    (throw (ex-info "Not a time" {:data time :kind ::not-time})))
  (let [fmt (DateTimeFormatter/ofPattern "HHmmss")
        zero (.format LocalTime/MIDNIGHT fmt)
        time-str (.format (unwrap time) fmt)
        time-str (if (= zero time-str)
                   ""
                   time-str)]
    (adx-write write-to time-str)))

(defn adx-with-negative [object]
  (if-not (s/valid? ::data-number? object)
    (throw (ex-info "Not a number" {:data object :kind ::not-number})))
  (apply-data - object))
(defn adx-with-not [object]
  (if-not (s/valid? ::data-boolean? object)
    (throw (ex-info "Not a boolean" {:data object :kind ::not-boolean})))
  (apply-data not object))
(defn adx-with-as-bool [object]
  (apply-data #(if % true false) object))

(defn adx-write-with-float-or-false [decimals, namespace, write-to, object]
  (if-not (or (s/valid? ::data-number? object) (= false (unwrap object)))
    (throw (ex-info "Not a number" {:data object :kind ::not-number})))
  (let [object (apply-data #(or % 0.0) object)]
    (adx-write write-to
               (String/format Locale/ENGLISH
                              (str "%." decimals "f")
                              (to-array [(double (unwrap object))])))))

(defn make-hachage []
  (let [r (Random.)
        a "ABCDEFGHJKLMNPQRSTUVWXYZ" ;; No I or O
        n (fn [r] (nth a (.nextInt r (count a))))
        i (fn [r] (+ 2 (.nextInt r 8)))]
    (str (n r)
         (n r)
         (i r)
         (n r)
         (n r)
         (n r)
         (i r)
         (i r)
         (i r)
         (i r))))

(defn adx-write-with-PFLR-amount [namespace write-to PFLR-value]
  ;; TODO: Verify its validity
  (let [real-amount (:amount (unwrap PFLR-value))
        amount (unwrap real-amount)
        real-number (:number (unwrap PFLR-value))
        number (unwrap real-number)]
    (cond (#{105 308 705 309 311 407} number)
          (adx-write-with-positive-float 2 namespace write-to real-amount)
          (#{102 103 104 116 404 405 409 410 502 503 504 505 506 507 709 806 808} real-number)
          (adx-write-with-positive-float 3 namespace write-to real-amount)
          true
          (adx-write-with-positive-float 3 namespace write-to real-amount))))

(defn adx-write-with-CFLP-tax-name [namespace write-to cflp-value]
  (cond (= cflp-value :tps)
        (adx-write-with-string namespace write-to "T.P.S.")
        (= cflp-value :tvq)
        (adx-write-with-string namespace write-to "T.V.Q.")
        true
        (throw (ex-info "Bad tax kind" {:data cflp-value
                                        :kind ::bad-tax-kind}))))
(defn adx-write-with-CFLP-tax-number [namespace write-to cflp-value]
  (cond (= cflp-value :tps)
        (adx-write-with-int namespace write-to 991)
        (= cflp-value :tvq)
        (adx-write-with-int namespace write-to 992)
        true
        (throw (ex-info "Bad tax kind" {:data cflp-value
                                        :kind ::bad-tax-kind}))))
(defn adx-write-with-CFLP-tax-number-23xx [namespace write-to cflp-value]
  (cond (= cflp-value :tps)
        (adx-write-with-int namespace write-to 2301)
        (= cflp-value :tvq)
        (adx-write-with-int namespace write-to 2311)
        true
        (throw (ex-info "Bad tax kind" {:data cflp-value
                                        :kind ::bad-tax-kind}))))

;; heures-ou-pourcent [:key, (Data amount)]
(defn adx-write-with-positive-float-variable [namespace write-to heures-ou-pourcent]
  (let [u (unwrap heures-ou-pourcent)]
    (if (not (vector? u))
      (adx-write-with-positive-float 2 namespace write-to u)
      (let [kind (first u)
            amount (second u)]
        (cond (= kind :two)
              (adx-write-with-positive-float 2 namespace write-to amount)
              (= kind :three)
              (adx-write-with-positive-float 3 namespace write-to amount)
              true
              (throw (ex-info "Bad heures-ou-pourcent" {:data heures-ou-pourcent
                                                        :kind ::bad-heures-ou-pourcent})))))))

(defn adx-write-with-space [namespace, write-to]
  (adx-write-space write-to))
(defn adx-write-with-separator [namespace, write-to]
  (adx-write-separator write-to))
(defn adx-write-with-separator-space [namespace, write-to]
  (adx-write-separator-space write-to))
(defn adx-write-with-new-line [namespace, write-to]
  (adx-write-new-line write-to))
(defn adx-write-with-new-line-indent [namespace, write-to]
  (adx-write-new-line-indent write-to))
(defn adx-write-with-increase-indent [namespace, write-to]
  (adx-write-increase-indent write-to))
(defn adx-write-with-decrease-indent [namespace, write-to]
  (adx-write-decrease-indent write-to))
(defn adx-write-with-container-start [namespace, write-to, container-name]
  (adx-write-container-start write-to container-name))
(defn adx-write-with-container-end [namespace, write-to]
  (adx-write-container-end write-to))

(defn get-from-container [container name]
  (if (= name :self)
    container
    (name (unwrap container))))
(defn check-undefined-type! [namespace type]
  (if-not (get namespace type)
    (throw (ex-info "Undefined type"
                    {:data type :kind ::undefined-type :pinternal true}))))
(defn get-namespace-type [namespace type]
  (check-undefined-type! namespace type)
  (get namespace type))
(defn adx-with-compose-fns [namespace fns]
  (if (keyword? fns)
    (get-namespace-type namespace fns)
    (reduce (fn [k0 k1]
              (fn [namespace write-to & args]
                ((if (keyword? k0) (get-namespace-type namespace k0) k0)
                 namespace write-to
                 (apply (get-namespace-type namespace k1) args)))) fns)))
(defn adx-write-with-type
  ([namespace, write-to, type]
   (adx-write-with-type namespace, write-to, type, {}))
  ([namespace, write-to, type, container]
   {:post [(s/valid? ::adx-write-to %)]}
   (if-not (s/valid? ::adx-namespace namespace)
     (throw (ex-info "Invalid namespace"
                     {:data namespace :kind ::not-namespace :internal true})))
   (if-not (s/valid? ::adx-write-to write-to)
     (throw (ex-info "Invalid write-to"
                     {:data write-to :kind ::not-write-to :internal true})))
   (if-not (s/valid? ::adx-type type)
     (throw (ex-info "Invalid type" {:data type :kind ::not-type})))
   (if-not (s/valid? ::adx-container (unwrap container))
     (throw (ex-info "Invalid container"
                     {:data container :kind ::not-container :internal true})))
   (cond
     (keyword? type)
     ((get-namespace-type namespace type) namespace write-to)
     (string? type)
     ((get-namespace-type namespace :string) namespace write-to type)
     (vector? type)
     (apply (adx-with-compose-fns namespace (first type)) namespace write-to
            (get-from-container container (second type)) (drop 2 type))
     (or (list? type) (seq? type))
     (apply (adx-with-compose-fns namespace (first type)) namespace write-to (rest type))
     true
     (throw (ex-info "Unreachable" {})))))

(defn adx-write-with-container
  ([ordering, namespace, write-to]
   (adx-write-with-container ordering, namespace, write-to, {}))
  ([ordering, namespace, write-to, container]
   {:post [(s/valid? ::adx-write-to %)]}
   (if-not (s/valid? ::adx-ordering ordering)
     (throw (ex-info "Invalid ordering"
                     {:data ordering :kind ::not-ordering :internal true})))
   (if-not (s/valid? ::adx-namespace namespace)
     (throw (ex-info "Invalid namespace"
                     {:data namespace :kind ::not-namespace :internal true})))
   (if-not (s/valid? ::adx-write-to write-to)
     (throw (ex-info "Invalid write-to"
                     {:data write-to :kind ::not-write-to :internal true})))
   (if-not (s/valid? ::adx-container (unwrap container))
     (throw (ex-info "Invalid container" {:data container :kind ::not-container})))
   (loop [ordering ordering
          write-to write-to]
     (if (empty? ordering)
       write-to
       (recur
        (rest ordering)
        (try
          (adx-write-with-type namespace write-to (first ordering) container)
          (catch ExceptionInfo ex
            (throw (ex-info "Failed to write ordering"
                            {:data ordering :kind ::in-container} ex)))))))))

(defn adx-write-with-list [separator, type, namespace, write-to, adx-list]
  ;; loop over list, writing to write-to, separated by separator, written using
  ;; adx-write-with-container
  {:post [(s/valid? ::adx-write-to %)]}
  (if-not (s/valid? ::adx-standalone-ordering separator)
    (throw (ex-info "Invalid separator"
                    {:data separator :kind ::not-separator :internal true})))
  (if-not (s/valid? ::adx-namespace namespace)
    (throw (ex-info "Invalid namespace"
                    {:data namespace :kind ::not-namespace :internal true})))
  (if-not (s/valid? ::adx-write-to write-to)
    (throw (ex-info "Invalid write-to"
                    {:data write-to :kind ::not-write-to :internal true})))
  (if-not (s/valid? ::adx-list (unwrap adx-list))
    (throw (ex-info "Invalid adx-list" {:data adx-list :kind ::not-adx-list})))
  (loop [list (unwrap adx-list)
         write-to write-to]
    (if (empty? list)
      write-to
      (recur
       (rest list)
       (let [written (try ((get-namespace-type namespace type)
                           namespace write-to (first list))
                          (catch ExceptionInfo ex
                            (throw (ex-info "Failed to write value in list"
                                            {:data (make-data list (origin adx-list)) :type type
                                             :kind ::in-list} ex))))]
         (if (> (count list) 1)
           (adx-write-with-container separator namespace written)
           written))))))

(defn adx-write-with-container-ordering [namespace, write-to, container, ordering]
  (adx-write-with-container ordering namespace write-to container))

(defn adx-write-with-list-ordering [namespace, write-to, adx-list, separator, ordering]
  (adx-write-with-list separator ::temp-container
                       (assoc namespace ::temp-container
                              (partial adx-write-with-container ordering))
                       write-to adx-list))

;; ORDERING
;; keyword = type to write
;; vector = user data [type user-key &rest args]
;; list = type to write with args (type &rest args)
;; other: 

(def SIDE-ordering
  '((:container-start "SIDE"),
    :separator-space,
    (:string "Acomba Data eXchange"),
    :separator-space,
    (:string " N"),
    :separator-space,
    [:string :company-name],
    :separator-space,
    :container-end
   ))

(def PEMP-ordering
  '((:container-start "PEMP"),
    :separator :increase-indent
    :new-line
    [:string :numero-id]
    :separator-space
    [:string :nas] :separator-space,
    :new-line
    [:string :cle-de-tri] :separator-space
    [:pint :sexe] :separator-space
    [:pint :etat-civil] :separator-space
    [:pint :langue] :separator-space
    [:date :date-de-naissance] :separator-space
    [:date :date-arrivee] :separator-space
    [:maybe-date :date-fin] :separator-space
    :separator-space
    (:pint 2) :separator
    :new-line
    (:pint 1) :separator-space
    [:pfloat2 :exemption-personelle-impot-federal] :separator-space
    (:pfloat2 0) :separator-space
    [:pfloat2 :deduction-supplementaire] :separator-space
    [:pfloat2 :deduction-region-eloignee] :separator-space
    [:pfloat2 :impot-additionel] :separator
    :new-line
    (:pint 1) :separator-space
    [:pfloat2 :exemption-personelle-impot-provincial] :separator-space
    (:pfloat3 100) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator
    :new-line
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    [:pint :code-assurance-emploi] :separator-space
    [:pint :code-rrq-rpc] :separator-space
    (:bool false) :separator-space ;; commentaire lors de l'inscription des paies
    [:bool :exemption-personelle-impot-federal-is-table] :separator-space
    [:bool :exemption-personelle-impot-provincial-is-table] :separator-space
    (:pint 1) :separator-space
    (:bool false) :separator
    :new-line
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    :separator-space :separator-space
    :new-line
    (:pint 1) :separator-space
    (:pfloat2 0) :separator-space
    (:pint 1) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    (:pint 0) :separator-space
    [:string :hachage-secret-PEMP] :separator-space
    :new-line
    (:pint 0) :separator-space :separator-space
    (:bool true) :separator-space
    (:bool true) :separator-space
    (:bool false) :separator-space
    (:bool true) :separator-space
    (:bool false) :separator
    :new-line
    :separator-space :separator-space :separator-space
    :separator-space :separator-space :separator
    :new-line
    [:SADT-container :SADT-container]
    :new-line
    [:SCBQ-container :SCBQ-container]
    :new-line
    [:PFON-container :PFON-container]
    :decrease-indent
    :new-line
    :container-end
   ))

(def SADT-ordering
  '((:container-start "SADT"),
   :separator :increase-indent
   :new-line
   [:string :nom-de-famille] :separator
   :new-line
   [:string :prenom] :separator
   :new-line
   [:string :rue] :separator
   :new-line
   [:string :ville] :separator
   :new-line
   [:string :code-postal] :separator
   :new-line
   :separator
   :new-line
   [:string :site-web] :separator
   :new-line
   [:string :courriel] :separator
   :new-line
   [:telephone :telephone] :separator
   :new-line
   [:telecopieur :telecopieur] :separator
   :new-line
   [:contact :contacte-0] :separator
   :new-line
   [:contact :contacte-1] :separator
   :new-line
   :separator [:pint :langue] :separator
   :new-line
   :separator
   :new-line
   :separator :separator
   :new-line
   :separator :separator
   :decrease-indent :new-line
   :container-end
   ))

(def telephone-ordering
  '([:string :phone]
    :separator
    [:string :post]
    ))
(def telecopieur-ordering
  '([:string :phone]
    ))
(def contact-ordering
  '([:string :nom]
    :separator
    [:string :phone]
    :separator
    [:string :post]
    ))

(def SCBQ-ordering
  '((:container-start "SCBQ")
    :separator
    [:string :institution] :separator
    [:string :succursale] :separator
    [:string :compte] :separator
    :container-end
    ))

(def PFON-ordering
  (list
    '(:container-start "PFON")
    :separator :increase-indent
    :new-line
    '(:pint 1) :separator-space
    [:string :nom-de-la-fonction] :separator-space
    [:pint :departement] :separator-space
    '(:bool true) :separator
    :new-line
    :separator
    :new-line
    ;; TODO: The next two ints are probably Vacances and Région from Définition
    '(:pint 0) :separator-space
    '(:pint 0) :separator-space
    [:date :date-de-debut] :separator-space :separator
    :new-line
    '(:pint 1) :separator-space
    '(:pfloat3 0) :separator-space
    '[:pint :vacances-mode]
    :separator-space
    '(:bool true) :separator-space
    '(:pfloat5 75) :separator-space :separator-space
    :new-line
    '(:pfloat2 0) :separator-space
    '(:pfloat2 0) :separator-space
    [:string :hachage-secret-PFON] :separator
    :new-line
    [:PFCO-list :PFCOs]
    :new-line
    [:PFLR-list :PFLRs]
    :new-line
    :PICS
    :new-line
    [:PFHR-list :PFHRs]
    :new-line
    :PEAE
    :new-line
    (list :PPRI-list (take 20 (repeat {})))
    :decrease-indent
    :new-line
    :container-end
    ))

(def PFCO-ordering
  '((:container-start "PFCO")
    :separator-space
    [:positive-int :numero-PFCO]
    :separator-space
    [:bool :oui-ou-non]
    :separator-space
    :container-end
    ))

(def PFLR-ordering
  '((:container-start "PFLR")
    :separator-space
    [:pint :number]
    :separator-space
    [:PFLR-amount :self]
    :separator-space
    [:pint :last-number]
    :separator-space
    :container-end
    ))

(def PICS-ordering
  '((:container-start "PICS")
    :separator-space :separator-space :separator-space
    (:string " ") :separator-space
    (:pint 0) :separator-space
    (:pint 0) :separator-space :separator-space
    (:string " ") :separator-space
    (:pint 1) :separator-space :separator-space :separator-space
    (:bool false) :separator-space
    (:pfloat3 0) :separator-space :separator-space
    :container-end
    ))

(def PFHR-ordering
  '((:container-start "PFHR")
    :separator-space
    [:pint :type-dheures] :separator-space
    [:pfloat2 :heures-regulieres] :separator-space
    :container-end
    ))

(def PEAE-ordering
  '((:container-start "PEAE")
    :separator
    :increase-indent :new-line
    (:pint 0) :separator-space
    (:pint 0) :separator-space
    ;; TODO: What's this 24?
    (:pint 24)
    :separator
    :decrease-indent
    :new-line
    :container-end
    ))

(def PPRI-ordering
  '((:container-start "PPRI")
    :separator
    (:pint 0) :separator
    (:pfloat2 0) :separator
    (:pint 0) :separator
    :container-end
    ))

(def PEMP-list-separator
  '(:new-line, :new-line))
(def PFCO-list-separator
  '(:new-line))
(def PFLR-list-separator
  '(:new-line))
(def PFHR-list-separator
  '(:new-line))
(def PPRI-list-separator
  '(:new-line))

(def SEOF-ordering
  '((:container-start "SEOF"),
   :separator-space,
   (:string "Acomba Data eXchange"),
   :separator-space,
   :container-end
   ))

(def employee-file-ordering
  '([:SIDE-container :SIDE],
   :new-line, :new-line,
   [:PEMP-list, :PEMPs],
   :new-line, :new-line,
   :new-line, :new-line,
   :SEOF, :new-line, :new-line,
   ))

(def PLNT-ordering
  '((:container-start "PLNT")
    :separator-space
    [:string :numero-id] :separator-space
    [:pint :fonction] :separator-space
    :separator-space
    [:date :date-punch-in] :separator-space
    [:date :date-punch-out] :separator-space
    [:time :time-punch-in] :separator-space
    [:time :time-punch-out] :separator-space
    [:pfloat3 :punch-duration] :separator-space
    (:pint 0) :separator-space
    [:pint :work-kind] :separator-space
    (:pint 0) :separator-space
    :separator-space
    [(:float2 :negative) :punch-duration] :separator-space
    (:pfloat3 0) :separator-space
    (:pfloat2 0) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space :separator
    :new-line
    :container-end
    ))

(def PLNT-list-separator
  '(:new-line))

(def punch-file-ordering
  '([:SIDE-container :SIDE],
   :new-line, :new-line,
   [:PLNT-list, :PLNTs],
   :new-line, :new-line,
   :new-line,
   :SEOF, :new-line, :new-line,
   ))

(def PPAI-ordering
  '((:container-start "PPAI")
    :separator :increase-indent :new-line
    [:string :numero-id] :separator-space
    [:pint :periode-paie] :separator-space
    [:date :date-paie-from] :separator-space
    [:date :date-paie-to] :separator-space
    [:date :date-paie-emis] :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    (:pint 0) :separator
    :new-line
    [:PSAL-container :PSAL]
    :decrease-indent :new-line
    :container-end
    ))

(def PPAI-list-separator '(:new-line, :new-line))

(def PSAL-ordering
  '((:container-start "PSAL")
    :separator :increase-indent :new-line
    (:pint 1) :separator-space ;; TODO: What is this 1?
    [:string :fonction-string] :separator-space
    :new-line
    (:pint 0) :separator-space
    ;; TODO: These numbers...
    ;; :montant-provisions-cat seems to be 410, so this is an unknown number
    [:pfloat2 :montant-provisions-cat] :separator-space
    (:pfloat2 0) :separator-space
    [:pfloat2 :montant-provisions-cat] :separator-space
    (:pfloat2 0) :separator-space
    [:pfloat2 :salaire-net-global] :separator-space
    (:pint 1) :separator-space
    (:pint 1) :separator-space
    (:pint 1) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat3 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space :separator
    :new-line
    [:pfloat2 :hours-week-0] :separator-space
    [:pfloat2 :pay-week-0] :separator-space
    (:bool false) :separator
    :new-line
    [:pfloat2 :hours-week-1] :separator-space
    [:pfloat2 :pay-week-1] :separator-space
    (:bool false) :separator
    :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    :separator :separator :new-line
    (:pint 0) :separator-space :separator
    :new-line
    (:pfloat2 0) :separator-space
    :new-line
    (:pfloat2 0) :separator-space
    :new-line
    :separator :separator :new-line
    (:pint 0) :separator-space
    (:pint 0) :separator
    :new-line
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:bool false) :separator-space
    (:bool false) :separator-space
    (:pfloat3 0) :separator
    :new-line
    :separator :separator
    :new-line
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator
    :new-line
    :paie-PICS
    :new-line
    [:PSLR-list :PSLRs]
    :new-line
    [:PSCU-container :PSCU]
    :new-line
    :PSDP
    :new-line
    :PSAP
    [:PLNT-list-newline :PLNTs]
    :decrease-indent :new-line :container-end
    ))

(def paie-PICS-ordering
  '((:container-start "PICS")
    :separator-space :separator-space :separator-space
    :separator-space
    (:pint 0) :separator-space
    (:pint 0) :separator-space :separator-space
    :separator-space
    (:pint 0) :separator-space :separator-space :separator-space
    :separator-space
    (:pfloat3 0) :separator-space :separator-space
    :container-end
    ))


(def PSLR-list-separator '(:new-line))

(def PSLR-ordering
  '((:container-start "PSLR") :separator-space
    [:pint :type-dheures] :separator-space
    [:pfloat-variable :heures-ou-pourcent] :separator-space
    [:pfloat-variable :montant] :separator-space
    [:pfloat2 :total] :separator-space :separator-space
    [:bool :manuel] :separator-space
    :container-end
    ))

(def PSCU-ordering
  '((:container-start "PSCU") :separator-space
    (:pint 1) :separator-space
    [:pfloat2 :vacances-accumulees] :separator-space
    :container-end
    ))

(def PSDP-ordering
  '(
    (:container-start "PSDP") :separator-space
    (:pint 0) :separator-space
    (:pfloat5 100) :separator-space
    :container-end
    ))

(def PSAP-ordering
  '((:container-start "PSAP") :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    (:pfloat2 0) :separator-space
    :container-end
    ))

(def paie-file-ordering
  '([:SIDE-container :SIDE],
    :new-line, :new-line,
    [:PPAI-list, :PPAIs],
    :new-line, :new-line,:new-line, :new-line,:new-line
    :SEOF, :new-line, :new-line,
    ))

(def CFLP-list-separator '(:new-line))

(def CFLP-comptes-clients-ordering
  '((:container-start "CFLP")
    :separator :increase-indent :new-line
    (:pint 2) :separator
    (:pint 0) :separator :separator
    :new-line
    :separator :separator :separator :new-line
    :separator :separator :separator :new-line
    (:string "Comptes clients") :separator :new-line
    (:pfloat5 0) :separator
    (:pfloat5 0) :separator
    (:pfloat4 0) :separator
    (:pfloat2 0) :separator
    (:pint 0) :separator :new-line
    (:pfloat4 0) :separator
    (:pfloat4 0) :separator
    (:bool false) :separator
    (:bool false) :separator
    (:bool false) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator :new-line
    :separator :new-line :separator :new-line
    :separator :separator
    (:pint 1201) :separator :separator
    (:bool false) :separator
    (:bool false) :separator :new-line
    :separator :separator :separator :separator :separator :new-line
    :separator :separator
    (:pint 0) :separator :new-line
    :separator
    (:pint 0) :separator :separator :separator :new-line
    :separator :new-line
    (:bool false) :separator-space :separator-space
    (:pint 0) :separator-space
    (:pint 0) :separator :new-line
    (:pfloat2 0) :separator
    :decrease-indent :new-line
    :container-end
    ))
(def CFLP-ordering
  '((:container-start "CFLP")
    :separator :increase-indent :new-line
    (:pint 1) :separator
    [:pint :line-number] :separator
    [:string :product-id] :separator :new-line
    :separator :separator :separator :new-line
    :separator :separator
    [:string :product-group-id] :separator :new-line
    [:string :product-description] :separator :new-line
    [:float5 :product-quantity] :separator
    [:float5 :product-quantity] :separator
    [:float4 :product-price-per] :separator
    (:pfloat2 0) :separator
    (:pint 0) :separator :new-line
    [:float4 :product-price-per] :separator
    [:float4 :product-dernier-coutant] :separator
    (:bool false) :separator
    (:bool false) :separator
    (:bool false) :separator
    ;; The below two are actually pretty important, because acomba may
    ;; recompute the taxes using these, which could invalidate the data if
    ;; they're wrong.
    [:pint :taux-de-tps] :separator ;; Taux de taxe TPS (0 = no tax, 1 = Qc, 3 = 13%, 4 = 12%)
    [:pint :taux-de-tvq] :separator ;; Taux de taxe TVQ (0 = no tax, 1 = Qc)
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator :new-line
    :separator :new-line :separator :new-line
    :separator :separator
    [:string :product-group-id-thousands] :separator :separator ;; TODO: better name
    (:bool false) :separator
    (:bool false) :separator :new-line
    [:float4 :tps-amount] :separator ;; use tax exemption to determine if this should be computed
    [:float4 :tvq-amount] :separator ;; use tax exemption to determine if this should be computed
    (:float4 0) :separator
    (:float4 0) :separator
    (:float4 0) :separator :new-line
    :separator :separator
    (:pint 0) :separator :new-line
    [:string :product-id] :separator
    (:pint 0) :separator :separator
    [:float4 :product-price-per] :separator ;; TODO: again? Not present in non-product lines
    :new-line
    (:pfloat5 0) :separator :new-line
    (:bool false) :separator-space :separator-space
    [:pint :original-line-number] :separator-space ;; Original line number,
                                                   ;; before line deletions and such.
    (:pint 0) :separator :new-line
    [:float2 :total-price] :separator
    :decrease-indent :new-line
    :container-end
    ))
(def CFLP-tax-ordering
  '((:container-start "CFLP")
    :separator :increase-indent :new-line
    (:pint 4) :separator
    [:CFLP-tax-number :tax-kind] :separator :separator :new-line
    :separator :separator :separator :new-line
    :separator :separator :separator :new-line
    :separator :new-line
    (:float5 0) :separator
    (:float5 0) :separator
    (:float4 0) :separator
    (:float2 0) :separator
    (:pint 0) :separator :new-line
    (:float4 0) :separator
    (:float4 0) :separator
    (:bool false) :separator
    (:bool false) :separator
    (:bool false) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator
    (:pint 0) :separator :new-line
    [:CFLP-tax-name :tax-kind] :separator :new-line
    :separator :new-line
    [:float4 :amount] :separator :separator
    [:CFLP-tax-number-23xx :tax-kind] :separator :separator
    (:bool false) :separator
    (:bool false) :separator :new-line
    :separator :separator :separator :separator :separator :new-line
    :separator :separator
    (:pint 0) :separator :new-line
    :separator
    (:pint 0) :separator :separator :separator :new-line
    :separator :new-line
    (:bool false) :separator-space :separator-space
    (:pint 0) :separator-space
    (:pint 0) :separator :new-line
    (:float2 0) :separator
    :decrease-indent :new-line
    :container-end
    ))

(def CFPR-ordering
  `((:container-start "CFPR")
    :separator :increase-indent :new-line
    [:string :client-id] :separator :separator
    [:pint :facture-number] :separator
    (:pint 1) :separator
    (:pint 0) :separator
    (:bool true) :separator
    (:bool true) :separator :new-line
    (:pint 1) :separator
    (:pint 9999) :separator :separator :separator :separator :separator
    :new-line
    [:date :facture-date] :separator :separator :separator
    :new-line
    :separator :new-line :separator :new-line
    :separator :separator
    (:bool true) :separator
    (:pint 10) :separator
    (:bool false) :separator
    (:bool false) :separator
    (:pfloat2 0) :separator
    (:pint 0) :separator
    (:bool false) :separator
    :new-line
    :separator-space
    [:pfloat2 :montant-paye] :separator-space
    (:bool false) :separator :new-line
    (:bool false) :separator
    (:pint 0) :separator :new-line
    :separator-space :separator-space
    (:bool true) :separator-space
    [:pfloat2 :montant-recu] :separator
    :new-line
    (:SADT-facture-container {:nom-de-famille "EL CLIENTE"})
    :new-line
    (:SADT-facture-container {:nom-de-famille ""})
    :new-line
    :CFLP-comptes-clients :new-line
    [:list :products-list ~CFLP-list-separator ~CFLP-ordering] :new-line
    [:container :tps-tax ~CFLP-tax-ordering] :new-line
    [:container :tvq-tax ~CFLP-tax-ordering] :new-line
    :CFVI
    :decrease-indent :new-line
    :container-end
    ))

(def CFPR-list-separator '(:new-line :new-line))

(def SADT-facture-ordering
  '((:container-start "SADT")
    :separator :increase-indent :new-line
    [:string :nom-de-famille] :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :new-line
    :separator :separator :new-line
    :separator :new-line
    :separator :separator :separator :new-line
    :separator :separator :separator
    :decrease-indent :new-line
    :container-end
    ))
(def CFVI-ordering
  '((:container-start "CFVI")
    :separator :increase-indent :new-line
    (:pint 1) :separator
    (:pfloat2 100) :separator
    :decrease-indent :new-line
    :container-end
    ))

(def CLNP-ordering
  '((:container-start "CLNP")
    :separator
    [:pint :facture-number] :separator
    [:float2 :facture-amount] :separator
    [:float2 :facture-escompte] :separator :separator
    (:bool true) :separator :separator
    (:pint 3) :separator ;; TODO: What is this? It's different for 1 of them.
    :container-end
  ))


(def CPAI-ordering
  `((:container-start "CPAI")
    :separator :increase-indent :new-line
    [:string :client-id] :separator
    [:date :payment-date] :separator
    (:pint 1) :separator
    [:pint :payment-number] :separator
    (:bool true) :separator :new-line
    [:pint :payment-type] :separator
    [:float2-or-false :payment-amount] :separator
    (:pint 0) :separator
    (:float2 0) :separator
    [:pint :payment-type-negative] :separator :new-line
    [:float2-or-false :payment-amount-negative] :separator
    (:float2 0) :separator :new-line
    (:pint 0) :separator
    [:date :payment-creation-date] :separator ;; TODO which date is which?
    [:date :payment-last-updated-date] :separator ;; TODO which date is which?
    ;; The following bool seems to say if the payment is associated to the facture.
    (:bool false) :separator :separator
    (:bool false) :separator
    (:bool false) :separator :new-line
    :separator :new-line
    [:float2 :cash-rounding] :separator :new-line
    [:container :associated-facture-info ~CLNP-ordering]
    :decrease-indent :new-line
    :container-end
    ))

(def CPAI-list-separator '(:new-line :new-line))

(def ITRA-ordering
  `((:container-start "ITRA")
    :separator :increase-indent :new-line
    [:pint :facture-ou-ajustement] :separator ;; 2 = facture, 3 = ajustement
    [:string :product-id] :separator
    [:float5 :product-adjustment] :separator :separator
    [:float4 :product-dernier-coutant] :separator :new-line
    :separator :separator :separator :new-line
    :separator :separator :new-line
    [:date :date-adjustment] :separator
    [:string :facture-id] :separator
    [:float4 :product-price-per] :separator
    (:pint 0) :separator :new-line
    :separator
    :decrease-indent :new-line
    :container-end
    ))

(def ITRA-list-separator
  '(:new-line :new-line))

(def facture-file-ordering
  `([:container :SIDE, ~SIDE-ordering],
    :new-line, :new-line,
    [:list, :ajuste-inventaire, ~ITRA-list-separator ~ITRA-ordering],
    :new-line, :new-line
    [:list, :factures, ~CFPR-list-separator ~CFPR-ordering],
    :new-line, :new-line,:new-line,
    [:list :paiements ~CPAI-list-separator ~CPAI-ordering],
    :new-line :new-line
    :SEOF, :new-line, :new-line,
    ))

(def core-namespace
  {
   :container adx-write-with-container-ordering,
   :list adx-write-with-list-ordering,
   :container-start adx-write-with-container-start,
   :container-end adx-write-with-container-end,
   :string adx-write-with-string,
   :int adx-write-with-int,
   :pint adx-write-with-positive-int,
   :pfloat2 (partial adx-write-with-positive-float 2)
   :pfloat3 (partial adx-write-with-positive-float 3)
   :pfloat4 (partial adx-write-with-positive-float 4)
   :pfloat5 (partial adx-write-with-positive-float 5)
   :float2 (partial adx-write-with-float 2)
   :float3 (partial adx-write-with-float 3)
   :float4 (partial adx-write-with-float 4)
   :float5 (partial adx-write-with-float 5)
   :bool adx-write-with-boolean
   :bool-numeric adx-write-with-boolean-numeric
   :date adx-write-with-date
   :time adx-write-with-time
   :maybe-date (partial adx-write-with-maybe adx-write-with-date)

   :negative adx-with-negative
   :not adx-with-not
   :as-bool adx-with-as-bool

   :float2-or-false (partial adx-write-with-float-or-false 2)

   :new-line adx-write-with-new-line-indent,
   :new-line-no-indent adx-write-with-new-line,
   :increase-indent adx-write-with-increase-indent,
   :decrease-indent adx-write-with-decrease-indent,
   :separator adx-write-with-separator,
   :space adx-write-with-space,
   :separator-space adx-write-with-separator-space,

   :SIDE-container (partial adx-write-with-container SIDE-ordering),
   :PEMP-list (partial adx-write-with-list PEMP-list-separator :PEMP-container),
   :PEMP-container (partial adx-write-with-container PEMP-ordering),
   :SADT-container (partial adx-write-with-container SADT-ordering),
   :telephone (partial adx-write-with-container telephone-ordering)
   :telecopieur (partial adx-write-with-container telecopieur-ordering)
   :contact (partial adx-write-with-container contact-ordering)
   :SCBQ-container (partial adx-write-with-container SCBQ-ordering),
   :PFON-container (partial adx-write-with-container PFON-ordering),
   :PFCO-list (partial adx-write-with-list PFCO-list-separator :PFCO-container),
   :PFCO-container (partial adx-write-with-container PFCO-ordering),
   :PFLR-list (partial adx-write-with-list PFLR-list-separator :PFLR-container),
   :PFLR-container (partial adx-write-with-container PFLR-ordering),
   :PFLR-amount adx-write-with-PFLR-amount
   :PICS (partial adx-write-with-container PICS-ordering),
   :PFHR-list (partial adx-write-with-list PFHR-list-separator :PFHR-container),
   :PFHR-container (partial adx-write-with-container PFHR-ordering),
   :PEAE (partial adx-write-with-container PEAE-ordering)
   :PPRI-list (partial adx-write-with-list PPRI-list-separator :PPRI-container),
   :PPRI-container (partial adx-write-with-container PPRI-ordering),
   :SEOF (partial adx-write-with-container SEOF-ordering),

   :PLNT-list (partial adx-write-with-list PLNT-list-separator :PLNT-container)
   :PLNT-container (partial adx-write-with-container PLNT-ordering)

   :PPAI-list (partial adx-write-with-list PPAI-list-separator :PPAI-container)
   :PPAI-container (partial adx-write-with-container PPAI-ordering)
   :PSAL-container (partial adx-write-with-container PSAL-ordering)
   :paie-PICS (partial adx-write-with-container paie-PICS-ordering)
   :PSLR-list (partial adx-write-with-list PSLR-list-separator :PSLR-container)
   :PSLR-container (partial adx-write-with-container PSLR-ordering)
   :pfloat-variable adx-write-with-positive-float-variable
   :PSCU-container (partial adx-write-with-container PSCU-ordering)
   :PSDP (partial adx-write-with-container PSDP-ordering)
   :PSAP (partial adx-write-with-container PSAP-ordering)
   :PLNT-list-newline (fn [namespace, write-to, adx-list]
                        (if (not (empty? adx-list))
                          ((:PLNT-list namespace) namespace
                           (adx-write-with-type namespace write-to :new-line)
                           adx-list)
                          write-to))
   :CFPR-container (partial adx-write-with-container CFPR-ordering)
   :CFPR-list (partial adx-write-with-list CFPR-list-separator :CFPR-container),
   :SADT-facture-container (partial adx-write-with-container SADT-facture-ordering)
   :CFLP-comptes-clients (partial adx-write-with-container CFLP-comptes-clients-ordering)
   :CFLP-container (partial adx-write-with-container CFLP-ordering)
   :CFLP-tax-container (partial adx-write-with-container CFLP-tax-ordering)
   :CFLP-tax-name adx-write-with-CFLP-tax-name,
   :CFLP-tax-number adx-write-with-CFLP-tax-number,
   :CFLP-tax-number-23xx adx-write-with-CFLP-tax-number-23xx,
   :CFLP-list (partial adx-write-with-list CFLP-list-separator :CFLP-container),
   :CFVI (partial adx-write-with-container CFVI-ordering)
   :CPAI-container (partial adx-write-with-container CPAI-ordering)
   :CPAI-list (partial adx-write-with-list CPAI-list-separator :CPAI-container),
   :CLNP-container (partial adx-write-with-container CLNP-ordering)
   })

(defn adx-write-to-clear [write-to]
  (assoc write-to ::str (StringBuilder.)))
(defn adx-write-to-empty? [write-to]
  (empty? (::str write-to)))

(defn core-ns-optimize-file-ordering-inner
  [ordering write-to core-namespace]
  (loop [write-to write-to
         cur-ordering ordering
         new-ordering []]
    (if (empty? cur-ordering)
      (if (adx-write-to-empty? write-to)
        [write-to new-ordering]
        [(adx-write-to-clear write-to)
         (conj new-ordering (adx-write-finish write-to))])
      (let [object (first cur-ordering)
            [write-to new-ordering]
            (cond
              (vector? object)
              (let [[write-to new-ordering]
                    (if (adx-write-to-empty? write-to)
                      [write-to new-ordering]
                      [(adx-write-to-clear write-to)
                       (conj new-ordering (adx-write-finish write-to))])]
                (cond (= :container (first object))
                      (let [[write-to, new-nth]
                            (core-ns-optimize-file-ordering-inner
                             (nth object 2) write-to core-namespace)]
                        [write-to (conj new-ordering (assoc object 2 new-nth))])
                      (= :list (first object))
                      (let [[write-to, new-nth]
                            (core-ns-optimize-file-ordering-inner
                             (nth object 3) write-to core-namespace)]
                        [write-to (conj new-ordering (assoc object 3 new-nth))])
                      true
                      [write-to
                       (conj new-ordering object)]))
              (or (= object :increase-indent) (= object :decrease-indent)) ;; these are special and must be preserved
              (let [write-to (adx-write-with-type core-namespace write-to object)]
                [(adx-write-to-clear write-to)
                 (conj new-ordering (adx-write-finish write-to) object)])
              (or (keyword? object) (string? object) (list? object) (seq? object))
              [(adx-write-with-type core-namespace write-to object)
               new-ordering]
              true
              (throw (ex-info "Bad value in ordering"
                              {:ordering ordering :value object})))]
        (recur write-to (rest cur-ordering) new-ordering)))))

;; Optimize orderings by pre-computing the stuff that can be, which is
;; basically everything that's not in a vector (vector is for user-declared
;; stuff). Nested orderings are also optimized if they're in a vector using the
;; :container or :list keywords.
(defn core-ns-optimize-file-ordering
  ([ordering]
   (core-ns-optimize-file-ordering core-namespace))
  ([ordering core-namespace]
   (let [[write-to new-ordering]
         (core-ns-optimize-file-ordering-inner ordering (new-adx-write-to) core-namespace)]
     (if-not (adx-write-to-empty? write-to)
       (throw (ex-info "Error: write-to not empty!"
                       {:write-to write-to})))
     new-ordering)))

(def facture-file-ordering-optimized
  (core-ns-optimize-file-ordering facture-file-ordering core-namespace))
