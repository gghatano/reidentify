SKIP  README.md:64 (install/help instructions)
SKIP  README.md:90 (install/help instructions)
SKIP  README.md:100 (install/help instructions)

===== RUN README.md:113 =====
  ---- README.md:113 +1  (no #> expected)
  ---- README.md:113 +7  (no #> expected)
  ---- README.md:113 +8  (no #> expected)
  ---- README.md:113 +10  (no #> expected)
  ---- README.md:113 +11  (no #> expected)
  OK   README.md:113 +13  <head(raw)>
  ---- README.md:113 +29  (no #> expected)
  ---- README.md:113 +32  (no #> expected)
  OK   README.md:113 +33  <reid_evaluate(scores, seeds = 1:20)>

===== RUN README.md:207 =====
  ---- README.md:207 +1  (no #> expected)
  ---- README.md:207 +2  (no #> expected)
  ---- README.md:207 +5  (no #> expected)
  ---- README.md:207 +6  (no #> expected)
  OK   README.md:207 +8  <head(picked, 5)>
  OK   README.md:207 +16  <reid_evaluate(combined, seeds = 1:20)>

===== RUN README.md:243 =====
  ---- README.md:243 +1  (no #> expected)
  OK   README.md:243 +2  <c(wrong         = nrow(wrong),   correct_below = sum(picked$>

===== RUN README.md:267 =====
  ---- README.md:267 +1  (no #> expected)
  ---- README.md:267 +3  (no #> expected)
  OK   README.md:267 +11  <names(m)>

===== RUN README.md:294 =====
  ---- README.md:294 +1  (no #> expected)
  ---- README.md:294 +2  (no #> expected)

===== RUN README.md:307 =====
  ---- README.md:307 +2  (no #> expected)
  ---- README.md:307 +3  (no #> expected)
  ---- README.md:307 +4  (no #> expected)
  ---- README.md:307 +5  (no #> expected)
  ---- README.md:307 +7  (no #> expected)
  ---- README.md:307 +9  (no #> expected)
  ---- README.md:307 +11  (no #> expected)
  OK   README.md:307 +13  <head(set_raw, 2)>
  OK   README.md:307 +17  <head(set_anon, 2)>

===== RUN README.md:338 =====
  ---- README.md:338 +1  (no #> expected)
  OK   README.md:338 +2  <attr(cand, "blocking")>

===== RUN README.md:350 =====
  ---- README.md:350 +1  (no #> expected)
  OK   README.md:350 +2  <c(full    = reid_evaluate(score_multi(pairs, qi, screen = "n>

===== RUN README.md:372 =====
  [WARNING: block_candidates(): blocking discarded 122 of 200 true pair(s) (recall 0.39). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:372 +1  (no #> expected)
  OK   README.md:372 +2  <attr(lossy, "blocking")>

===== RUN README.md:387 =====
  OK   README.md:387 +1  <reid_evaluate(score_multi(lossy, qi, screen = "none"), seeds>

===== RUN README.md:412 =====
  [WARNING: block_candidates(): blocking discarded 24 of 200 true pair(s) (recall 0.88). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  OK   README.md:412 +1  <attr(block_candidates(raw, anon, keys = "AGE",              >
  OK   README.md:412 +6  <attr(block_candidates(raw, anon, keys = list("ZIP", "AGE")),>

===== RUN README.md:427 =====
  [WARNING: lsh_candidates(): blocking discarded 23 of 200 true pair(s) (recall 0.885). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").]
  ---- README.md:427 +1  (no #> expected)
  OK   README.md:427 +2  <attr(blocked, "blocking")>

===== RUN README.md:448 =====
  OK   README.md:448 +1  <attr(top_k_candidates(score_multi(pairs, qi), k = 10), "bloc>

===== RUN README.md:461 =====
  OK   README.md:461 +1  <blocking_recall(cand, raw, anon)$kept_fraction>

===== RUN README.md:505 =====
  OK   README.md:505 +1  <c(dist    = reid_evaluate(score_dist(set_pairs, "ITEMS"), se>

===== RUN README.md:529 =====
  ---- README.md:529 +1  (no #> expected)
  OK   README.md:529 +4  <h>
  OK   README.md:529 +9  <generalize_value(c(31, 37, 46), "AGE", h, levels = 1)>

===== RUN README.md:542 =====
  ---- README.md:542 +1  (no #> expected)
  ---- README.md:542 +2  (no #> expected)
  ---- README.md:542 +4  (no #> expected)
  OK   README.md:542 +8  <g_anon>
  ---- README.md:542 +17  (no #> expected)
  OK   README.md:542 +20  <containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)>
  OK   README.md:542 +36  <match_greedy(score_containment(g_pairs, c("AGE", "AREA"), hi>

===== RUN README.md:591 =====
  OK   README.md:591 +1  <tryCatch(score_char(g_pairs, "AGE"),          error = functi>

===== RUN README.md:601 =====
  OK   README.md:601 +1  <is_generalized_value(c("37", "30s", "[30,40)", "135****", "M>

===== RUN README.md:617 =====
  OK   README.md:617 +1  <head(value_frequencies(pairs, "ZIP"), 3)>
  OK   README.md:617 +7  <reid_evaluate(score_idf_match(pairs, c("ZIP", "SEX")), seeds>

===== RUN README.md:642 =====
  OK   README.md:642 +1  <c(char = reid_evaluate(score_char(pairs, "ZIP"), seeds = 1:2>

===== RUN README.md:662 =====
  ---- README.md:662 +1  (no #> expected)
  OK   README.md:662 +3  <reid_evaluate(score_count(m_pairs), seeds = 1:10)>

===== RUN README.md:692 =====
  ---- README.md:692 +1  (no #> expected)
  OK   README.md:692 +2  <reid_evaluate(s_multi, seeds = 1:20)>

===== RUN README.md:714 =====
  [WARNING: combine_scores(): the components are on very different scales -- scores[[1]] has 26.1x the weighted spread of scores[[2]] (sd 13.8 vs 0.531). The widest component decides the ranking and the others only break its ties. That is harmless when the dominant component is also the most informative, but when it is not, adding attributes LOWERS the measured reidentification rate and the result understates the risk. Put the components on a common scale first -- combine_scores(normalize_scores(scores, "range")) or score_multi() -- or set weights to compensate. Pass scale_check = "none" if the scale gap is intended.]
  OK   README.md:714 +2  <vapply(list(   raw_sum    = combine_scores(list(s_age, s_zip>

===== RUN README.md:744 =====
  OK   README.md:744 +1  <axis_report(s_multi)>

===== RUN README.md:781 =====
  OK   README.md:781 +1  <head(score_mahalanobis(pairs, c("AGE", "VISIT_COUNT")), 3)>

===== RUN README.md:800 =====
  ---- README.md:800 +2  (no #> expected)
  OK   README.md:800 +4  <c(greedy  = mean(match_greedy(ranked, seed = 1)$RESULT),   o>

===== RUN README.md:832 =====
  OK   README.md:832 +1  <head(reid_confidence(combined), 3)>

===== RUN README.md:859 =====
  OK   README.md:859 +1  <stats::quantile(reid_confidence(combined)$CONFIDENCE, c(0.5,>

===== RUN README.md:874 =====
  ---- README.md:874 +1  (no #> expected)
  ---- README.md:874 +4  (no #> expected)
  ---- README.md:874 +5  (no #> expected)
  ---- README.md:874 +6  (no #> expected)
  OK   README.md:874 +8  <match_scoreboard_rh(score_scoreboard(sb_pairs, c("I1", "I2",>

===== RUN README.md:927 =====
  ---- README.md:927 +1  (no #> expected)
  OK   README.md:927 +4  <reid_stability(attack_num, pairs, "AGE", seeds = 1:20)>

===== RUN README.md:949 =====
  ---- README.md:949 +1  (no #> expected)
  OK   README.md:949 +5  <k>
  OK   README.md:949 +10  <reid_evaluate(score_by_knowledge(pairs, k), seeds = 1:20)>

===== RUN README.md:978 =====
  OK   README.md:978 +1  <reid_knowledge_curve(   pairs,   quasi_identifiers = c(AGE =>

===== RUN README.md:1007 =====
  OK   README.md:1007 +1  <unicity_fraction(raw, c("AGE", "ZIP"))>
  OK   README.md:1007 +4  <unicity(raw, attributes = c("AGE", "ZIP", "SEX"), p = 1:3, s>

===== RUN README.md:1030 =====
  ---- README.md:1030 +1  (no #> expected)
  OK   README.md:1030 +3  <spatiotemporal_unicity(st, k = c(1, 2, 4), time_resolution =>

===== RUN README.md:1053 =====
  OK   README.md:1053 +1  <coarsen_place(c("P001", "P002", "P003", "P004"), resolution >
  OK   README.md:1053 +3  <coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)>

===== RUN README.md:1239 =====
  ---- README.md:1239 +1  (no #> expected)
  OK   README.md:1239 +2  <c(success = unique(e$per_seed$success)[1], trial = unique(e$>
SKIP  README.md:1264 (install/help instructions)

==== 37 R block(s) run, 4 skipped; 47 output unit(s) compared, 0 mismatching ====
==== 4 warning(s)/message(s) signalled, 0 pinned expectation(s) violated ====
==== 50 exported function(s), 0 missing from README ====
