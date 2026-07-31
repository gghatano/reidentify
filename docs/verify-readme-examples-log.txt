SKIP  README.md:52 (install/help instructions)
SKIP  README.md:78 (install/help instructions)
SKIP  README.md:88 (install/help instructions)

===== RUN README.md:101 =====
  ---- README.md:101 +1  (no #> expected)
  ---- README.md:101 +7  (no #> expected)
  ---- README.md:101 +8  (no #> expected)
  ---- README.md:101 +10  (no #> expected)
  ---- README.md:101 +11  (no #> expected)
  OK   README.md:101 +13  <head(raw)>
  ---- README.md:101 +29  (no #> expected)
  ---- README.md:101 +32  (no #> expected)
  OK   README.md:101 +33  <reid_evaluate(scores, seeds = 1:20)>

===== RUN README.md:195 =====
  ---- README.md:195 +1  (no #> expected)
  ---- README.md:195 +2  (no #> expected)
  ---- README.md:195 +5  (no #> expected)
  ---- README.md:195 +6  (no #> expected)
  OK   README.md:195 +8  <head(picked, 5)>
  OK   README.md:195 +16  <reid_evaluate(combined, seeds = 1:20)>

===== RUN README.md:231 =====
  ---- README.md:231 +1  (no #> expected)
  OK   README.md:231 +2  <c(wrong         = nrow(wrong),   correct_below = sum(picked$>

===== RUN README.md:248 =====
  [WARNING: combine_scores(): the components are on very different scales -- scores[[1]] has 26.1x the weighted spread of scores[[2]] (sd 13.8 vs 0.531). The widest component decides the ranking and the others only break its ties. That is harmless when the dominant component is also the most informative, but when it is not, adding attributes LOWERS the measured reidentification rate and the result understates the risk. Put the components on a common scale first -- combine_scores(normalize_scores(scores, "range")) or score_multi() -- or set weights to compensate. Pass scale_check = "none" if the scale gap is intended.]
  OK   README.md:248 +2  <vapply(list(   raw_sum    = combine_scores(list(s_age, s_zip>

===== RUN README.md:273 =====
  ---- README.md:273 +1  (no #> expected)
  OK   README.md:273 +4  <print(reid_result(r, method = "AGE"))>

===== RUN README.md:298 =====
  ---- README.md:298 +1  (no #> expected)
  OK   README.md:298 +2  <reid_evaluate(s_multi, seeds = 1:20)>

===== RUN README.md:325 =====
  OK   README.md:325 +1  <axis_report(s_multi)>

===== RUN README.md:362 =====
  OK   README.md:362 +1  <head(score_mahalanobis(pairs, c("AGE", "VISIT_COUNT")), 3)>

===== RUN README.md:381 =====
  OK   README.md:381 +1  <head(value_frequencies(pairs, "ZIP"), 3)>
  OK   README.md:381 +7  <reid_evaluate(score_idf_match(pairs, c("ZIP", "SEX")), seeds>

===== RUN README.md:406 =====
  OK   README.md:406 +1  <c(char = reid_evaluate(score_char(pairs, "ZIP"), seeds = 1:2>

===== RUN README.md:423 =====
  ---- README.md:423 +2  (no #> expected)
  ---- README.md:423 +3  (no #> expected)
  ---- README.md:423 +4  (no #> expected)
  ---- README.md:423 +5  (no #> expected)
  ---- README.md:423 +7  (no #> expected)
  ---- README.md:423 +9  (no #> expected)
  ---- README.md:423 +11  (no #> expected)
  OK   README.md:423 +13  <head(set_raw, 2)>
  OK   README.md:423 +17  <head(set_anon, 2)>
  OK   README.md:423 +22  <c(dist    = reid_evaluate(score_dist(set_pairs, "ITEMS"), se>

===== RUN README.md:470 =====
  ---- README.md:470 +1  (no #> expected)
  OK   README.md:470 +2  <attr(cand, "blocking")>

===== RUN README.md:482 =====
  ---- README.md:482 +1  (no #> expected)
  OK   README.md:482 +2  <c(full    = reid_evaluate(score_multi(pairs, qi, screen = "n>

===== RUN README.md:504 =====
  [WARNING: block_candidates(): blocking discarded 122 of 200 true pair(s) (recall 0.39). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:504 +1  (no #> expected)
  OK   README.md:504 +2  <attr(lossy, "blocking")>

===== RUN README.md:519 =====
  OK   README.md:519 +1  <reid_evaluate(score_multi(lossy, qi, screen = "none"), seeds>

===== RUN README.md:544 =====
  [WARNING: block_candidates(): blocking discarded 24 of 200 true pair(s) (recall 0.88). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  OK   README.md:544 +1  <attr(block_candidates(raw, anon, keys = "AGE",              >
  OK   README.md:544 +6  <attr(block_candidates(raw, anon, keys = list("ZIP", "AGE")),>

===== RUN README.md:559 =====
  [WARNING: lsh_candidates(): blocking discarded 23 of 200 true pair(s) (recall 0.885). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:559 +1  (no #> expected)
  OK   README.md:559 +2  <attr(blocked, "blocking")>

===== RUN README.md:580 =====
  OK   README.md:580 +1  <attr(top_k_candidates(s_multi, k = 10), "blocking")>

===== RUN README.md:593 =====
  OK   README.md:593 +1  <blocking_recall(cand, raw, anon)$kept_fraction>

===== RUN README.md:628 =====
  ---- README.md:628 +1  (no #> expected)
  OK   README.md:628 +4  <h>
  OK   README.md:628 +9  <generalize_value(c(31, 37, 46), "AGE", h, levels = 1)>

===== RUN README.md:641 =====
  ---- README.md:641 +1  (no #> expected)
  ---- README.md:641 +2  (no #> expected)
  ---- README.md:641 +4  (no #> expected)
  OK   README.md:641 +8  <g_anon>
  ---- README.md:641 +17  (no #> expected)
  OK   README.md:641 +20  <containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)>
  OK   README.md:641 +36  <match_greedy(score_containment(g_pairs, c("AGE", "AREA"), hi>

===== RUN README.md:690 =====
  OK   README.md:690 +1  <tryCatch(score_char(g_pairs, "AGE"),          error = functi>

===== RUN README.md:700 =====
  OK   README.md:700 +1  <is_generalized_value(c("37", "30s", "[30,40)", "135****", "M>

===== RUN README.md:722 =====
  ---- README.md:722 +1  (no #> expected)
  ---- README.md:722 +4  (no #> expected)
  ---- README.md:722 +5  (no #> expected)
  ---- README.md:722 +6  (no #> expected)
  OK   README.md:722 +8  <match_scoreboard_rh(score_scoreboard(sb_pairs, c("I1", "I2",>

===== RUN README.md:757 =====
  ---- README.md:757 +2  (no #> expected)
  OK   README.md:757 +4  <c(greedy  = mean(match_greedy(ranked, seed = 1)$RESULT),   o>

===== RUN README.md:791 =====
  OK   README.md:791 +1  <head(reid_confidence(combined), 3)>

===== RUN README.md:818 =====
  OK   README.md:818 +1  <stats::quantile(reid_confidence(combined)$CONFIDENCE, c(0.5,>

===== RUN README.md:844 =====
  OK   README.md:844 +1  <reid_stability(reid_by_num, pairs, "AGE", seeds = 1:20)>

===== RUN README.md:865 =====
  ---- README.md:865 +1  (no #> expected)
  OK   README.md:865 +5  <k>
  OK   README.md:865 +10  <reid_evaluate(score_by_knowledge(pairs, k), seeds = 1:20)>

===== RUN README.md:894 =====
  OK   README.md:894 +1  <reid_knowledge_curve(   pairs,   quasi_identifiers = c(AGE =>

===== RUN README.md:925 =====
  OK   README.md:925 +1  <unicity_fraction(raw, c("AGE", "ZIP"))>
  OK   README.md:925 +4  <unicity(raw, attributes = c("AGE", "ZIP", "SEX"), p = 1:3, s>

===== RUN README.md:948 =====
  ---- README.md:948 +1  (no #> expected)
  OK   README.md:948 +3  <spatiotemporal_unicity(st, k = c(1, 2, 4), time_resolution =>

===== RUN README.md:971 =====
  OK   README.md:971 +1  <coarsen_place(c("P001", "P002", "P003", "P004"), resolution >
  OK   README.md:971 +3  <coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)>

===== RUN README.md:985 =====
  ---- README.md:985 +1  (no #> expected)
  ---- README.md:985 +3  (no #> expected)
  OK   README.md:985 +11  <names(m)>

===== RUN README.md:1018 =====
  ---- README.md:1018 +1  (no #> expected)
  OK   README.md:1018 +3  <reid_evaluate(score_count(m_pairs), seeds = 1:10)>

===== RUN README.md:1040 =====
  ---- README.md:1040 +1  (no #> expected)
  ---- README.md:1040 +2  (no #> expected)
SKIP  README.md:1223 (install/help instructions)

==== 36 R block(s) run, 4 skipped; 47 output unit(s) compared, 0 mismatching ====
==== 4 warning(s)/message(s) signalled, 0 pinned expectation(s) violated ====
==== 55 exported function(s), 0 missing from README ====
