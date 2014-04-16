(ns cljs.tools.reader.tests
  (:refer-clojure :exclude [read-string *default-data-reader-fn*])
  (:use-macros [cljs.tools.reader.test-helpers
                :only [deftest is is= testing throws?]])
  (:use [cljs.tools.reader :only [read-string *default-data-reader-fn*]]))

(deftest read-integer
  (is= 42 (read-string "42"))
  (is= +42 (read-string "+42"))
  (is= -42 (read-string "-42"))

  (is= 42 (read-string "42N"))
  (is= +42 (read-string "+42N"))
  (is= -42 (read-string "-42N"))

  (is= 0 (read-string "0"))

  (is= 042 (read-string "042"))
  (is= +042 (read-string "+042"))
  (is= -042 (read-string "-042"))

  (is= 0x42e (read-string "0x42e"))
  (is= +0x42e (read-string "+0x42e"))
  (is= -0x42e (read-string "-0x42e")))

(deftest read-floating
  (is= 42.23 (read-string "42.23"))
  (is= +42.23 (read-string "+42.23"))
  (is= -42.23 (read-string "-42.23"))

  (is= 42.2e3 (read-string "42.2e3"))
  (is= +42.2e+3 (read-string "+42.2e+3"))
  (is= -42.2e-3 (read-string "-42.2e-3")))

(deftest read-ratio
  (is= 4/2 (read-string "4/2"))
  (is= 4/2 (read-string "+4/2"))
  (is= -4/2 (read-string "-4/2")))


(deftest read-symbol
  (is= 'foo (read-string "foo"))
  (is= 'foo/bar (read-string "foo/bar"))
  (is= '*+!-_? (read-string "*+!-_?"))
  (is= 'abc:def:ghi (read-string "abc:def:ghi"))
  (is= 'abc.def/ghi (read-string "abc.def/ghi"))
  (is= 'abc/def.ghi (read-string "abc/def.ghi"))
  (is= 'abc:def/ghi:jkl.mno (read-string "abc:def/ghi:jkl.mno"))
  (is (instance? clojure.lang.Symbol (read-string "alphabet")))
  (is= "foo//" (str (read-string "foo//"))) ;; the clojure reader can't read this
  (is= (str 'NaN) (str (read-string "NaN"))) ;; the clojure reader can't read this
  (is= Infinity (read-string "Infinity")) ;; the clojure reader can't read this
  (is= +Infinity (read-string "+Infinity")) ;; the clojure reader can't read this
  (is= -Infinity (read-string "-Infinity"))) ;; the clojure reader can't read this

(deftest read-specials
  (is= 'nil nil)
  (is= 'false false)
  (is= 'true true))

(deftest read-char
  (is= \f (read-string "\\f"))
  (is= \u0194 (read-string "\\u0194"))
  (is= \a (read-string "\\x61")) ;; the clojure reader can't read this
  (is= \o123 (read-string "\\o123"))
  (is= \newline (read-string "\\newline"))
  (is= (char 0) (read-string "\\o0"))
  (is= (char 0) (read-string "\\o000"))
  (is= (char 0377) (read-string "\\o377"))
  (is= \A (read-string "\\u0041"))
  (is= \@ (read-string "\\@"))
  (is= (char 0xd7ff) (read-string "\\ud7ff"))
  (is= (char 0xe000) (read-string "\\ue000"))
  (is= (char 0xffff) (read-string "\\uffff")))

(deftest read-string*
  (is= "foo bar" (read-string "\"foo bar\""))
  (is= "foo\\bar" (read-string "\"foo\\\\bar\""))
  (is= "foo\000bar" (read-string "\"foo\\000bar\""))
  (is= "foo\u0194bar" (read-string "\"foo\\u0194bar\""))
  (is= "fooabar" (read-string "\"foo\\x61bar\"")) ;; the clojure reader can't read this
  (is= "foo\123bar" (read-string "\"foo\\123bar\"")))

(deftest read-list
  (is= '() (read-string "()"))
  (is= '(foo bar) (read-string "(foo bar)"))
  (is= '(foo (bar) baz) (read-string "(foo (bar) baz)")))

(deftest read-vector
  (is= '[] (read-string "[]"))
  (is= '[foo bar] (read-string "[foo bar]"))
  (is= '[foo [bar] baz] (read-string "[foo [bar] baz]")))

(deftest read-map
  (is= '{} (read-string "{}"))
  (is= '{foo bar} (read-string "{foo bar}"))
  (is= '{foo {bar baz}} (read-string "{foo {bar baz}}")))

(deftest read-set
  (is= '#{} (read-string "#{}"))
  (is= '#{foo bar} (read-string "#{foo bar}"))
  (is= '#{foo #{bar} baz} (read-string "#{foo #{bar} baz}")))

(deftest read-metadata
  (is= {:foo true} (meta (read-string "^:foo 'bar")))
  (is= {:foo 'bar} (meta (read-string "^{:foo bar} 'baz")))
  (is= {:tag "foo"} (meta (read-string "^\"foo\" 'bar")))
  (is= {:tag 'String} (meta (read-string "^String 'x"))))


(deftest read-keyword
  (is= :foo-bar (read-string ":foo-bar"))
  (is= :foo/bar (read-string ":foo/bar"))
  (is= :*+!-_? (read-string ":*+!-_?"))
  (is= :abc:def:ghi (read-string ":abc:def:ghi"))
  (is= :abc.def/ghi (read-string ":abc.def/ghi"))
  (is= :abc/def.ghi (read-string ":abc/def.ghi"))
  (is= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno"))
  (is (instance? cljs.lang.Keyword (read-string ":alphabet"))) )

(deftest read-regex
  (is= (str #"\[\]?(\")\\")
         (str (read-string "#\"\\[\\]?(\\\")\\\\\""))))

(deftest read-quote
  (is= ''foo (read-string "'foo")))

(deftest read-deref
  (is= '@foo (read-string "@foo")))

(deftest read-var
  (is= '(var foo) (read-string "#'foo")))

(deftest read-fn
  (is= '(fn* [] (foo bar baz)) (read-string "#(foo bar baz)")))

;(deftest read-arg
;  (is= 14 ((eval (read-string "#(apply + % %1 %3 %&)")) 1 2 3 4 5))
;  (is= 4 ((eval (read-string "#(last %&)")) 1 2 3 4)))


(deftest read-tagged
  ;; (is= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst \"2010-11-12T13:14:15.666\""))
  ;; (is= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst\"2010-11-12T13:14:15.666\""))
  ;; (is= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\""))
  ;; (is= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\""))
  (is= #uuid "550e8400-e29b-41d4-a716-446655440000"
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\""))
  (is= #uuid "550e8400-e29b-41d4-a716-446655440000"
         (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\""))
  (when *default-data-reader-fn*
    (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
      (is= {:unknown-tag 'foo :value 'bar}
             (binding [*default-data-reader-fn* my-unknown]
               (read-string "#foo bar"))))))

(defrecord foo [])
(defrecord bar [baz buz])

(deftest read-record
  (is= (foo.) (read-string "#clojure.tools.reader_test.foo[]"))
  (is= (foo.) (read-string "#clojure.tools.reader_test.foo []")) ;; not valid in clojure
  (is= (foo.) (read-string "#clojure.tools.reader_test.foo{}"))
  (is= (assoc (foo.) :foo 'bar) (read-string "#clojure.tools.reader_test.foo{:foo bar}"))

  (is= (map->bar {}) (read-string "#clojure.tools.reader_test.bar{}"))
  (is= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar{:baz 1}"))
  (is= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar[1 nil]"))
  (is= (bar. 1 2) (read-string "#clojure.tools.reader_test.bar[1 2]")))

(deftest read-ctor
  (is= "foo" (read-string "#java.lang.String[\"foo\"]")))
