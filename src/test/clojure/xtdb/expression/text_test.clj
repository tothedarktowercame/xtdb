(ns xtdb.expression.text-test
  (:require [clojure.test :as t :refer [deftest]]
            [xtdb.api :as xt]
            [xtdb.compactor :as c]
            [xtdb.db-catalog :as db]
            [xtdb.expression-test :as et]
            [xtdb.expression.metadata :as expr.meta]
            [xtdb.test-util :as tu]
            [xtdb.trie :as trie]
            [xtdb.util :as util]
            [xtdb.vector.writer :as vw])
  (:import (xtdb.text TextSearch)
           (xtdb.trie Trie)))

(t/use-fixtures :each tu/with-mock-clock tu/with-node)
(t/use-fixtures :once tu/with-allocator)

(deftest test-tokenize
  (t/is (= ["the" "quick" "brown" "fox"]
           (vec (TextSearch/tokenize "The quick, BROWN — fox!"))))

  (t/is (= [] (vec (TextSearch/tokenize "  ... !!"))))
  (t/is (= [] (vec (TextSearch/tokenize ""))))

  (t/is (= ["v2" "of" "the"  "api"]
           (vec (TextSearch/tokenize "v2 of the API"))))

  ;; case-folded but no diacritic folding (stated v0 contract)
  (t/is (= ["café" "au" "lait"]
           (vec (TextSearch/tokenize "Café au LAIT")))))

(deftest test-text-match-expr
  (t/is (true? (et/project1 '(text-match "The quick brown fox" "fox") {})))
  (t/is (true? (et/project1 '(text-match "The quick brown fox" "FOX quick") {})))

  ;; token match, not substring/prefix match
  (t/is (false? (et/project1 '(text-match "The quick brown fox" "foxes") {})))
  (t/is (false? (et/project1 '(text-match "quixotic" "qui") {})))

  ;; AND semantics: every needle term must be present
  (t/is (false? (et/project1 '(text-match "The quick brown fox" "fox dog") {})))

  ;; a needle with no tokens matches vacuously
  (t/is (true? (et/project1 '(text-match "anything at all" "") {})))
  (t/is (true? (et/project1 '(text-match "anything at all" " !! ") {})))

  ;; non-literal needle (runtime tokenisation path)
  (t/is (true? (et/project1 '(text-match x y) {:x "sphinx of black quartz", :y "QUARTZ"})))
  (t/is (false? (et/project1 '(text-match x y) {:x "sphinx of black quartz", :y "zebra"}))))

(deftest test-text-match-sql-e2e
  (xt/execute-tx tu/*node* [[:put-docs :docs {:xt/id 1, :body "The quick brown fox jumps over the lazy dog"}]
                            [:put-docs :docs {:xt/id 2, :body "Pack my box with five dozen liquor jugs"}]
                            [:put-docs :docs {:xt/id 3, :body "Sphinx of black quartz, judge my vow"}]])

  (let [fox-q "SELECT _id FROM docs WHERE text_match(body, 'fox') ORDER BY _id"]
    ;; live/L0 tier: no text blooms yet — answers must come from recheck alone
    (t/is (= [{:xt/id 1}] (xt/q tu/*node* fox-q)))

    (tu/flush-block! tu/*node*)
    (c/compact-all! tu/*node* #xt/duration "PT1S")

    ;; compacted: text blooms in play, same answers
    (t/is (= [{:xt/id 1}] (xt/q tu/*node* fox-q))))

  (t/is (= [{:xt/id 2} {:xt/id 3}]
           (xt/q tu/*node* "SELECT _id FROM docs WHERE text_match(body, 'my') ORDER BY _id")))

  ;; AND semantics + case-insensitivity
  (t/is (= [{:xt/id 1}]
           (xt/q tu/*node* "SELECT _id FROM docs WHERE text_match(body, 'DOG lazy')")))

  (t/is (= [] (xt/q tu/*node* "SELECT _id FROM docs WHERE text_match(body, 'fox liquor')")))

  ;; param needle: exercises the :param branch of the pruning hashes
  (t/is (= [{:xt/id 3}]
           (xt/q tu/*node* ["SELECT _id FROM docs WHERE text_match(body, ?)" "quartz sphinx"]))))

(deftest test-text-match-historical
  ;; the all-versions claim (#5637): tries retain every version, so the
  ;; term bloom indexes all versions and text predicates work under time travel
  (xt/execute-tx tu/*node* [[:put-docs :notes {:xt/id 1, :body "an ephemeral remark"}]])
  (xt/execute-tx tu/*node* [[:put-docs :notes {:xt/id 1, :body "a durable remark"}]])

  (tu/flush-block! tu/*node*)
  (c/compact-all! tu/*node* #xt/duration "PT1S")

  ;; as-of-now: the superseded version must NOT match…
  (t/is (= [] (xt/q tu/*node* "SELECT _id FROM notes WHERE text_match(body, 'ephemeral')")))
  (t/is (= [{:xt/id 1}] (xt/q tu/*node* "SELECT _id FROM notes WHERE text_match(body, 'durable')")))

  ;; …but is still findable across system time
  (t/is (= [{:xt/id 1}]
           (xt/q tu/*node* "SELECT _id FROM notes FOR ALL SYSTEM_TIME WHERE text_match(body, 'ephemeral')"))))

(deftest test-text-bloom-page-pruning
  (xt/execute-tx tu/*node* [[:put-docs :docs {:xt/id 1, :body "The quick brown fox"}]
                            [:put-docs :docs {:xt/id 2, :body "Pack my box with five dozen liquor jugs"}]])

  (tu/flush-block! tu/*node*)
  (c/compact-all! tu/*node* #xt/duration "PT1S")

  (let [metadata-mgr (.getMetadataManager (db/primary-db tu/*node*))
        ->selector (fn [needle]
                     (expr.meta/->metadata-selector tu/*allocator*
                                                    (list 'text-match 'body needle)
                                                    '{body #xt/field {"body" :utf8}}
                                                    vw/empty-args))]

    (t/testing "L0 (no blooms calculated) — must not prune"
      (let [meta-file-path (Trie/metaFilePath #xt/table docs ^String (trie/->l0-trie-key 0))]
        (util/with-open [page-metadata (.openPageMetadataSync metadata-mgr meta-file-path)]
          (t/is (true? (.test (.build (->selector "zebra") page-metadata) 0))
                "absent term: page still fetched, recheck owns correctness"))))

    (t/testing "L1 (compacted, blooms on)"
      (let [meta-file-path (Trie/metaFilePath #xt/table docs ^String (trie/->l1-trie-key nil 0))]
        (util/with-open [page-metadata (.openPageMetadataSync metadata-mgr meta-file-path)]
          (t/is (true? (.test (.build (->selector "fox") page-metadata) 0))
                "present term: page kept")
          (t/is (true? (.test (.build (->selector "FOX Quick") page-metadata) 0))
                "analysis chain shared with the writer: case-folded terms still hit")
          (t/is (false? (.test (.build (->selector "zebra") page-metadata) 0))
                "absent term: page pruned")
          (t/is (false? (.test (.build (->selector "fox zebra") page-metadata) 0))
                "AND semantics: one absent term prunes the page")
          (t/is (true? (.test (.build (->selector "") page-metadata) 0))
                "no needle tokens: vacuous match, page kept"))))))
