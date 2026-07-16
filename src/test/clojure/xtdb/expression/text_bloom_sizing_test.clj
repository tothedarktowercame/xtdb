(ns xtdb.expression.text-bloom-sizing-test
  "Empirical false-positive rates for the tokenised per-page term bloom
   (#5637), settling the sizing question in dev/doc/text-indexing.adoc:
   the term bloom reuses the value bloom's BLOOM_BITS=22/BLOOM_K=5, but holds
   many more distinct entries per page (tokens vs whole values).

   Builds blooms over n distinct tokens through the same write path the
   ColumnMetadata writer uses (BloomBuilder.add over UTF-8 token bytes),
   probes with tokens known to be absent via the same query path
   (TextSearch/tokenHashes), and reports FP rate + serialized size.

   The assertion pins the conclusion; the printed sweep carries the data."
  (:require [clojure.test :as t])
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets)
           (xtdb.bloom BloomBuilder BloomUtils)
           (xtdb.text TextSearch)))

(defn- ->token-buf ^ByteBuffer [^String s]
  (ByteBuffer/wrap (.getBytes s StandardCharsets/UTF_8)))

(defn- measure [^long n-tokens ^long n-probes]
  (let [builder (BloomBuilder.)]
    (dotimes [i n-tokens]
      (.add builder (->token-buf (str "present" i "x"))))
    (let [bloom (.build builder)
          fps (->> (range n-probes)
                   (filter #(BloomUtils/contains bloom (TextSearch/tokenHashes (str "absent" % "z"))))
                   count)]
      {:n-tokens n-tokens
       :fp-rate (double (/ fps n-probes))
       :serialized-bytes (.serializedSizeInBytes bloom)})))

(t/deftest text-bloom-false-positive-rates
  (let [probes 20000
        sweep (mapv #(measure % probes) [1000 10000 50000 100000 200000])]
    (println "text-bloom FP sweep (BLOOM_BITS=22, BLOOM_K=5," probes "probes):")
    (doseq [{:keys [n-tokens fp-rate serialized-bytes]} sweep]
      (println (format "  %7d tokens/page  fp-rate %.5f  bloom %6d bytes"
                       n-tokens fp-rate serialized-bytes)))

    ;; a text-heavy page holds on the order of 10^4–10^5 distinct tokens;
    ;; the inherited sizing must stay comfortably useful there
    (t/is (< (:fp-rate (nth sweep 3)) 0.001)
          "≤0.1% false positives at 100k distinct tokens/page")
    (t/is (< (:fp-rate (nth sweep 4)) 0.005)
          "≤0.5% false positives at 200k distinct tokens/page")))
