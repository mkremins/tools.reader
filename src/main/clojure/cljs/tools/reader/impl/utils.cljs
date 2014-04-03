;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki cljs.tools.reader.impl.utils
  (:refer-clojure :exclude [char]))

(defn char [x]
  (when x
    (cljs.core/char x)))

(defn ex-info? [ex]
  (instance? ExceptionInfo ex))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (#{\space \newline \tab \return} ch)
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (not (js/isNaN (js/parseInt ch)))))

(defn comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (identical? \;  ch))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))

(defn desugar-meta
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    :else        f))
