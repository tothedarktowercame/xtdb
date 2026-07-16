(ns xtdb.expression.text
  "`text_match` (#5637 v0): the authoritative recheck matcher.

   All-terms-present (AND) semantics over `xtdb.text.TextSearch`'s analysis chain
   (lowercase, split on non-letter/digit, no stemming). A needle with no tokens
   matches vacuously.

   The pruning counterpart is `:test-text-bloom` in `xtdb.expression.metadata`:
   it consults a tokenised per-page bloom and may only ever over-approximate
   this predicate — correctness always lands back here."
  (:require [xtdb.expression :as expr])
  (:import (xtdb.text TextSearch)))

(defmethod expr/codegen-call [:text_match :utf8 :utf8] [{[_ {:keys [literal]}] :args}]
  {:return-type #xt/type :bool
   :->call-code (fn [[haystack-code needle-code]]
                  `(boolean
                    ~(if literal
                       `(TextSearch/textMatchTokens (expr/resolve-string ~haystack-code)
                                                    ~(vec (TextSearch/tokenize literal)))
                       `(TextSearch/textMatch (expr/resolve-string ~haystack-code)
                                              (expr/resolve-string ~needle-code)))))})
