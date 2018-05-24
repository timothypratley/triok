(ns triok.util
  (:require [clojure.spec.alpha :as s]))

(defn task? [x] true)
(defn scope? [x] true)
(def body? (s/* any?))

(defn args? [x]
  (s/and vector? (fn args-are-symbols [x]
                   (every? symbol? x))))

(def conjs (fnil conj #{}))

(defmacro function [fn-name doc args arg-specs & body]
  (let [arg-symbols (remove #{'&} args)]
    (when (not= (count arg-symbols) (count arg-specs))
      ;; TODO: can this be a spec?
      (throw (ex-info "Arg count does not match spec count" {})))
    `(do
       (defn ~fn-name ~doc ~args
         ~@body)
       (s/fdef ~fn-name :args (s/cat ~@(interleave (map keyword arg-symbols) arg-specs))))))
(s/fdef function :args (s/cat :fn-name symbol?
                              :doc string?
                              :args args?
                              :spec any?                    ;;TODO
                              :body body?))

(defmacro macro [macro-name doc args arg-specs & body]
  (let [arg-symbols (remove #{'&} args)]
    (when (not= (count arg-symbols) (count arg-specs))
      ;; TODO: can this be a spec?
      (throw (ex-info "Arg count does not match spec count" {})))
    `(do
       (defmacro ~macro-name ~doc ~args
         ~@body)
       (s/fdef ~macro-name :args (s/cat ~@(interleave (map keyword arg-symbols) arg-specs))))))
(s/fdef macro :args (s/cat :macro-name symbol?
                           :doc string?
                           :args args?
                           :spec any?
                           :body body?))
