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
  ---- README.md:195 +4  (no #> expected)
  ---- README.md:195 +5  (no #> expected)
  OK   README.md:195 +7  <head(picked, 5)>
  OK   README.md:195 +15  <reid_evaluate(combined, seeds = 1:20)>

===== RUN README.md:239 =====
  ---- README.md:239 +1  (no #> expected)
  OK   README.md:239 +4  <print(reid_result(r, method = "AGE"))>

===== RUN README.md:264 =====
  ---- README.md:264 +1  (no #> expected)
  OK   README.md:264 +2  <reid_evaluate(s_multi, seeds = 1:20)>

===== RUN README.md:291 =====
  OK   README.md:291 +1  <axis_report(s_multi)>

===== RUN README.md:307 =====
  OK   README.md:307 +1  <head(score_mahalanobis(pairs, c("AGE", "VISIT_COUNT")), 3)>

===== RUN README.md:326 =====
  OK   README.md:326 +1  <head(value_frequencies(pairs, "ZIP"), 3)>
  OK   README.md:326 +7  <reid_evaluate(score_idf_match(pairs, c("ZIP", "SEX")), seeds>

===== RUN README.md:351 =====
  OK   README.md:351 +1  <c(char = reid_evaluate(score_char(pairs, "ZIP"), seeds = 1:2>

===== RUN README.md:368 =====
  ---- README.md:368 +2  (no #> expected)
  ---- README.md:368 +3  (no #> expected)
  ---- README.md:368 +4  (no #> expected)
  ---- README.md:368 +5  (no #> expected)
  ---- README.md:368 +7  (no #> expected)
  ---- README.md:368 +9  (no #> expected)
  ---- README.md:368 +11  (no #> expected)
  OK   README.md:368 +13  <head(set_raw, 2)>
  OK   README.md:368 +17  <head(set_anon, 2)>
  OK   README.md:368 +22  <c(dist    = reid_evaluate(score_dist(set_pairs, "ITEMS"), se>

===== RUN README.md:415 =====
  ---- README.md:415 +1  (no #> expected)
  OK   README.md:415 +2  <attr(cand, "blocking")>

===== RUN README.md:427 =====
  ---- README.md:427 +1  (no #> expected)
  OK   README.md:427 +2  <c(full    = reid_evaluate(score_multi(pairs, qi, screen = "n>

===== RUN README.md:449 =====
  [WARNING: block_candidates(): blocking discarded 122 of 200 true pair(s) (recall 0.39). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:449 +1  (no #> expected)
  OK   README.md:449 +2  <attr(lossy, "blocking")>

===== RUN README.md:464 =====
  OK   README.md:464 +1  <reid_evaluate(score_multi(lossy, qi, screen = "none"), seeds>

===== RUN README.md:489 =====
  [WARNING: block_candidates(): blocking discarded 24 of 200 true pair(s) (recall 0.88). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  OK   README.md:489 +1  <attr(block_candidates(raw, anon, keys = "AGE",              >
  OK   README.md:489 +6  <attr(block_candidates(raw, anon, keys = list("ZIP", "AGE")),>

===== RUN README.md:504 =====
  [WARNING: lsh_candidates(): blocking discarded 23 of 200 true pair(s) (recall 0.885). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:504 +1  (no #> expected)
  OK   README.md:504 +2  <attr(blocked, "blocking")>

===== RUN README.md:525 =====
  OK   README.md:525 +1  <attr(top_k_candidates(s_multi, k = 10), "blocking")>

===== RUN README.md:538 =====
  OK   README.md:538 +1  <blocking_recall(cand, raw, anon)$kept_fraction>

===== RUN README.md:573 =====
  ---- README.md:573 +1  (no #> expected)
  OK   README.md:573 +4  <h>
  OK   README.md:573 +9  <generalize_value(c(31, 37, 46), "AGE", h, levels = 1)>

===== RUN README.md:586 =====
  ---- README.md:586 +1  (no #> expected)
  ---- README.md:586 +2  (no #> expected)
  ---- README.md:586 +4  (no #> expected)
  OK   README.md:586 +8  <g_anon>
  ---- README.md:586 +17  (no #> expected)
  OK   README.md:586 +20  <containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)>
  OK   README.md:586 +36  <match_greedy(score_containment(g_pairs, c("AGE", "AREA"), hi>

===== RUN README.md:635 =====
  OK   README.md:635 +1  <tryCatch(score_char(g_pairs, "AGE"),          error = functi>

===== RUN README.md:645 =====
  OK   README.md:645 +1  <is_generalized_value(c("37", "30s", "[30,40)", "135****", "M>

===== RUN README.md:667 =====
  ---- README.md:667 +1  (no #> expected)
  ---- README.md:667 +4  (no #> expected)
  ---- README.md:667 +5  (no #> expected)
  ---- README.md:667 +6  (no #> expected)
  OK   README.md:667 +8  <match_scoreboard_rh(score_scoreboard(sb_pairs, c("I1", "I2",>

===== RUN README.md:702 =====
  OK   README.md:702 +1  <c(greedy  = mean(match_greedy(combined, seed = 1)$RESULT),  >

===== RUN README.md:733 =====
  OK   README.md:733 +1  <head(reid_confidence(combined), 3)>

===== RUN README.md:759 =====
  OK   README.md:759 +1  <stats::quantile(reid_confidence(combined)$CONFIDENCE, c(0.5,>

===== RUN README.md:785 =====
  OK   README.md:785 +1  <reid_stability(reid_by_num, pairs, "AGE", seeds = 1:20)>

===== RUN README.md:806 =====
  ---- README.md:806 +1  (no #> expected)
  OK   README.md:806 +5  <k>
  OK   README.md:806 +10  <reid_evaluate(score_by_knowledge(pairs, k), seeds = 1:20)>

===== RUN README.md:835 =====
  OK   README.md:835 +1  <reid_knowledge_curve(   pairs,   quasi_identifiers = c(AGE =>

===== RUN README.md:866 =====
  OK   README.md:866 +1  <unicity_fraction(raw, c("AGE", "ZIP"))>
  OK   README.md:866 +4  <unicity(raw, attributes = c("AGE", "ZIP", "SEX"), p = 1:3, s>

===== RUN README.md:889 =====
  ---- README.md:889 +1  (no #> expected)
  OK   README.md:889 +3  <spatiotemporal_unicity(st, k = c(1, 2, 4), time_resolution =>

===== RUN README.md:912 =====
  OK   README.md:912 +1  <coarsen_place(c("P001", "P002", "P003", "P004"), resolution >
  OK   README.md:912 +3  <coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)>

===== RUN README.md:926 =====
  ---- README.md:926 +1  (no #> expected)
  ---- README.md:926 +3  (no #> expected)
  OK   README.md:926 +11  <names(m)>

===== RUN README.md:959 =====
  ---- README.md:959 +1  (no #> expected)
  OK   README.md:959 +3  <reid_evaluate(score_count(m_pairs), seeds = 1:10)>

===== RUN README.md:981 =====
  ---- README.md:981 +1  (no #> expected)
  ---- README.md:981 +2  (no #> expected)
SKIP  README.md:1164 (install/help instructions)

==== 34 R block(s) run, 4 skipped; 45 output unit(s) compared, 0 mismatching ====
==== 55 exported function(s), 0 missing from README ====
