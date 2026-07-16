(ns xtdb.expression.text-property-test
  "Oracle property test for text_match (#5637).

   Generates a random op sequence (puts/deletes/erases over a small id pool,
   bodies drawn from a vocabulary that exercises the analysis chain: casing,
   punctuation separators, accents, CJK, Cyrillic, non-BMP letters), then
   asserts that text_match through the real index path — live and compacted,
   as-of-now and FOR ALL SYSTEM_TIME — agrees with a naive oracle that applies
   the same matcher to a replayed document history.

   This machine-checks the design invariants (dev/doc/text-indexing.adoc):
   I-1 (bloom pruning may only over-approximate — a pruning false negative
   shows up here as a missing id after compaction), I-3 (deletes/erases need
   no index handling), I-4 (one analysis chain end-to-end)."
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xtdb.api :as xt]
            [xtdb.compactor :as c]
            [xtdb.node :as xtn]
            [xtdb.test-util :as tu])
  (:import (xtdb.text TextSearch)))

(def ^:private vocab
  ["fox" "dog" "quick" "brown" "lazy" "jumps" "box" "jugs" "sphinx"
   "café" "naïve" "北京" "мир" "v2" "α42" "𝔉𝔬𝔵"])

(def ^:private gen-body-token
  (gen/fmap (fn [[w up?]] (if up? (str/upper-case w) w))
            (gen/tuple (gen/elements vocab) gen/boolean)))

(def ^:private gen-doc-body
  (gen/fmap (fn [[tokens sep]] (str/join sep tokens))
            (gen/tuple (gen/vector gen-body-token 0 12)
                       (gen/elements [" " ", " " — " "!\n" " ☕ "]))))

(def ^:private doc-ids (vec (range 1 9)))

(def ^:private gen-op
  (gen/frequency [[8 (gen/fmap (fn [[id body]] [:put id body])
                               (gen/tuple (gen/elements doc-ids) gen-doc-body))]
                  [2 (gen/fmap (fn [id] [:delete id]) (gen/elements doc-ids))]
                  [1 (gen/fmap (fn [id] [:erase id]) (gen/elements doc-ids))]]))

(def ^:private gen-needle
  (gen/fmap (fn [[tokens up?]]
              (cond-> (str/join " " tokens) up? str/upper-case))
            (gen/tuple (gen/vector (gen/elements (conj vocab "absentterm")) 1 3)
                       gen/boolean)))

(defn- replay
  "Oracle state: :current = id → live body; :history = id → every body the id
   has ever had, surviving deletes (still visible FOR ALL SYSTEM_TIME) but not
   erases."
  [ops]
  (reduce (fn [acc [op id body]]
            (case op
              :put (-> acc
                       (assoc-in [:current id] body)
                       (update-in [:history id] (fnil conj #{}) body))
              :delete (update acc :current dissoc id)
              :erase (-> acc
                         (update :current dissoc id)
                         (update :history dissoc id))))
          {:current {}, :history {}}
          ops))

(defn- matching-ids [id->bodies needle]
  (->> id->bodies
       (filter (fn [[_ bodies]] (some #(TextSearch/textMatch % needle) bodies)))
       (map key)
       set))

(defn- q-ids [node sql needle]
  (->> (xt/q node [sql needle])
       (map :xt/id)
       set))

(t/deftest ^:property text-match-agrees-with-oracle
  (tu/run-property-test
   {:num-tests tu/property-test-iterations}
   (prop/for-all [ops (gen/vector gen-op 5 40)
                  needle gen-needle]
     (with-open [node (xtn/start-node {:log [:in-memory {:instant-src (tu/->mock-clock)}]})]
       ;; ensure the table exists before a leading delete/erase; the seed body
       ;; shares no tokens with the vocabulary, and needles always carry at
       ;; least one real token, so it can never appear in a result
       (xt/execute-tx node [[:put-docs :docs {:xt/id 0, :body "seedxyzq"}]])

       (doseq [[op id body] ops]
         (xt/execute-tx node [(case op
                                :put [:put-docs :docs {:xt/id id, :body body}]
                                :delete [:delete-docs :docs id]
                                :erase [:erase-docs :docs id])]))

       (let [{:keys [current history]} (replay ops)
             now-expected (matching-ids (update-vals current hash-set) needle)
             all-expected (matching-ids history needle)
             now-q "SELECT DISTINCT _id FROM docs WHERE text_match(body, ?)"
             all-q "SELECT DISTINCT _id FROM docs FOR ALL SYSTEM_TIME WHERE text_match(body, ?)"

             live-now (q-ids node now-q needle)
             live-all (q-ids node all-q needle)]

         (tu/flush-block! node)
         (c/compact-all! node #xt/duration "PT1S")

         (and (t/testing "live, as-of-now"
                (= now-expected live-now))
              (t/testing "live, all system time"
                (= all-expected live-all))
              (t/testing "compacted (term blooms active), as-of-now"
                (= now-expected (q-ids node now-q needle)))
              (t/testing "compacted (term blooms active), all system time"
                (= all-expected (q-ids node all-q needle)))))))))
