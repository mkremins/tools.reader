;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cljs.tools.reader.reader-types-macros)

(defmacro update! [what f]
  (list 'set! what (list f what)))

(defmacro log-source
  "If reader implements SourceLoggingReader, execute body in a source
  logging context. Otherwise, execute body, returning the result."
  [reader & body]
  `(if (and (source-logging-reader? ~reader)
            (not (whitespace? (peek-char ~reader))))
     (log-source* ~reader (^{:once true} fn* [] ~@body) false)
     (do ~@body)))

(defmacro log-source-unread
  "If reader implements SourceLoggingReader, execute body in a source
  logging context. Otherwise, execute body, returning the result."
  [reader & body]
  `(if (and (source-logging-reader? ~reader)
            (not (whitespace? (peek-char ~reader))))
     (log-source* ~reader (^{:once true} fn* [] ~@body) true)
     (do ~@body)))
