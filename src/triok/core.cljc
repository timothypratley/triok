(ns triok.core
  (:refer-clojure :exclude [await])
  (:require [triok.util :refer [macro function task? scope? body? args? conjs]]
            [task.core :as t]
            [clojure.spec.alpha :as s]))

;; don't touch me
(def ^{:dynamic true, :doc "implementation detail, do not use"} *async* nil)

(defn must-be-async [label]
  (when-not *async*
    (throw (ex-info (str "TRIO: " label " must be called in an async context.") {}))))

;; TODO: Use http://blog.klipse.tech/clojure/2016/10/10/defn-args-2.html
;; TODO: might want an fn form for inlining? or a macro form?
(macro async-function
  "the body of the async with block acts like an initial task that’s running inside the nursery,
  and then each call to nursery.start_soon adds another task that runs in parallel."
  [fn-name args & body]
  [symbol? args? body?]
  `(defn ~fn-name ~args
     (must-be-async "async-function")
     (let [p# (promise)
           env# (get-thread-bindings)
           ;; TODO: do we want all bindings, or only the *async* binding?
           task# (t/effect-off (with-bindings env# ~@body))
           cancel# (task# #(deliver p# %) #(deliver p# %))]
       (swap! *async* update :tasks conjs cancel#)
       ;; TODO: should the task also have a timeout?
       #_(if (timeout? *async*)
           (t/race))
       p#)))

(function current-time
  "Gets epoch"
  []
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn expired? [now deadline]
  (and (pos? deadline)
       (>= now deadline)))

;; TODO: I see much pain colliding with Clojure await
(function await
  "Waits for and returns the result of an async function."
  [async-fn & args]
  [task? (s/* any?)]
  (must-be-async "await")
  (let [{:keys [deadline]} @*async*
        now (current-time)]
    (if (and deadline (expired? now deadline))
      ::cancelled
      (let [p (apply async-fn args)]
        (swap! *async* update :promises conjs p)
        (if (pos? deadline)
          (deref p (- deadline now) ::cancelled)
          (deref p))))))

;; TODO: should take fn & args?
(function run
  "Returns whatever async-fn returns."
  [async-fn & args]
  [task? (s/* any?)]
  (when *async*
    (throw (ex-info "TRIO: run must be called synchronously" {})))
  ;; TODO: atom might not be quite right... use a ref??
  ;; TODO: does this collide with the cancel scope? should they be different?
  (binding [*async* (atom {})]
    (apply await async-fn args)))

;; TODO: should it support naming the scope or not?
;; TODO: deadline should be optional and default to infinity
(macro open-cancel-scope
  ""
  [deadline & body]
  [int? body?]
  `(let [scope# (atom {:deadline ~deadline
                       :cancel-called false
                       :cancel-caught false})]
     (if (expired? (current-time) ~deadline)
       (swap! scope# assoc :cancel-caught true)
       (binding [*async* scope#]
         ~@body))
     scope#))

(function cancel
  "Cancels a timeout scope"
  [scope]
  [scope?]
  (swap! scope assoc :cancel-called true)
  ;; TODO: not sure that I need to keep track of both promises and tasks...
  ;; seems like a good idea incase the task doesn't cancel (but maybe that's the implementation anyhow).
  (doseq [p (:promises @scope)]
    (deliver p ::cancelled))
  (doseq [t (:tasks @scope)]
    ;; cancel the task by calling it
    (t)))

(macro move-on-at
  "Creates a cancelable scope."
  [deadline & body]
  [int? body?]
  `(do
     (must-be-async "move-on-after")
     (open-cancel-scope ~deadline ~@body)))


;; TODO: if tasks complete before timeout, don't wait!
(macro move-on-after
  "Creates a cancelable scope."
  [ms & body]
  [int? body?]
  ;; TODO: this is wrong... will be current time at compile time
  `(move-on-at ~(+ (current-time) ms) ~@body))

(macro with-nursery
  ""
  []
  [])

(defn start-soon [nursery child]
  (t/join (constantly nil) nursery child))

(defn sleep [ms]
  @(t/timeout ms nil))
(s/fdef sleep :args (s/cat :ms int?))

(defn sleep-until [epoc]
  (let [t (- epoc (current-time))]
    (when (pos? t)
      (sleep t))))
(s/fdef sleep-until :args (s/cat :epoc int?))

(defn sleep-forever []
  @(t/timeout -1))
