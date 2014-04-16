(ns cljs.tools.reader.test-helpers)

(defmacro deftest
  [nm & body]
  `(do (.log js/console (str "Testing: " ~(str nm) "..."))
       ~@body))

(defmacro throws?
  [& exprs]
  `(try ~@exprs false
        (catch ~'js/Object e# true)))

(defmacro testing
  [nm & body]
    `(do (.log js/console (str "    " ~nm "..."))
       ~@body))

(defmacro is=
  [a b]
  `(let [a# ~a
         b# ~b]
     (assert (= a# b#) (str a# " != " b#))))

(defmacro is
  [a]
  `(assert ~a))
