SKIP  README.md:51 (install/help instructions)
SKIP  README.md:77 (install/help instructions)
SKIP  README.md:87 (install/help instructions)

===== RUN README.md:100 =====
  ---- README.md:100 +1  (no #> expected)
  ---- README.md:100 +7  (no #> expected)
  ---- README.md:100 +8  (no #> expected)
  ---- README.md:100 +10  (no #> expected)
  ---- README.md:100 +11  (no #> expected)
  OK   README.md:100 +13  <head(raw)>
  ---- README.md:100 +29  (no #> expected)
  ---- README.md:100 +32  (no #> expected)
  OK   README.md:100 +33  <reid_evaluate(scores, seeds = 1:20)>

===== RUN README.md:194 =====
  ---- README.md:194 +1  (no #> expected)
  ---- README.md:194 +2  (no #> expected)
  ---- README.md:194 +4  (no #> expected)
  ---- README.md:194 +5  (no #> expected)
  OK   README.md:194 +7  <head(picked, 5)>
  OK   README.md:194 +15  <reid_evaluate(combined, seeds = 1:20)>

===== RUN README.md:238 =====
  ---- README.md:238 +1  (no #> expected)
  OK   README.md:238 +4  <print(reid_result(r, method = "AGE"))>

===== RUN README.md:263 =====
  ---- README.md:263 +1  (no #> expected)
  OK   README.md:263 +2  <reid_evaluate(s_multi, seeds = 1:20)>

===== RUN README.md:290 =====
  OK   README.md:290 +1  <axis_report(s_multi)>

===== RUN README.md:306 =====
  OK   README.md:306 +1  <head(score_mahalanobis(pairs, c("AGE", "VISIT_COUNT")), 3)>

===== RUN README.md:325 =====
  OK   README.md:325 +1  <head(value_frequencies(pairs, "ZIP"), 3)>
  OK   README.md:325 +7  <reid_evaluate(score_idf_match(pairs, c("ZIP", "SEX")), seeds>

===== RUN README.md:350 =====
  OK   README.md:350 +1  <c(char = reid_evaluate(score_char(pairs, "ZIP"), seeds = 1:2>

===== RUN README.md:367 =====
  ---- README.md:367 +2  (no #> expected)
  ---- README.md:367 +3  (no #> expected)
  ---- README.md:367 +4  (no #> expected)
  ---- README.md:367 +5  (no #> expected)
  ---- README.md:367 +7  (no #> expected)
  ---- README.md:367 +9  (no #> expected)
  ---- README.md:367 +11  (no #> expected)
  OK   README.md:367 +13  <head(set_raw, 2)>
  OK   README.md:367 +17  <head(set_anon, 2)>
  OK   README.md:367 +22  <c(dist    = reid_evaluate(score_dist(set_pairs, "ITEMS"), se>

===== RUN README.md:403 =====
  ---- README.md:403 +1  (no #> expected)
  OK   README.md:403 +2  <attr(blocked, "blocking")>

===== RUN README.md:441 =====
  ---- README.md:441 +1  (no #> expected)
  OK   README.md:441 +4  <h>
  OK   README.md:441 +9  <generalize_value(c(31, 37, 46), "AGE", h, levels = 1)>

===== RUN README.md:454 =====
  ---- README.md:454 +1  (no #> expected)
  ---- README.md:454 +2  (no #> expected)
  ---- README.md:454 +4  (no #> expected)
  OK   README.md:454 +8  <g_anon>
  ---- README.md:454 +17  (no #> expected)
  OK   README.md:454 +20  <containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)>
  OK   README.md:454 +36  <match_greedy(score_containment(g_pairs, c("AGE", "AREA"), hi>

===== RUN README.md:503 =====
  OK   README.md:503 +1  <tryCatch(score_char(g_pairs, "AGE"),          error = functi>

===== RUN README.md:513 =====
  OK   README.md:513 +1  <is_generalized_value(c("37", "30s", "[30,40)", "135****", "M>

===== RUN README.md:535 =====
  ---- README.md:535 +1  (no #> expected)
  ---- README.md:535 +4  (no #> expected)
  ---- README.md:535 +5  (no #> expected)
  ---- README.md:535 +6  (no #> expected)
  OK   README.md:535 +8  <match_scoreboard_rh(score_scoreboard(sb_pairs, c("I1", "I2",>

===== RUN README.md:570 =====
  OK   README.md:570 +1  <c(greedy  = mean(match_greedy(combined, seed = 1)$RESULT),  >

===== RUN README.md:601 =====
  OK   README.md:601 +1  <head(reid_confidence(combined), 3)>

===== RUN README.md:627 =====
  OK   README.md:627 +1  <stats::quantile(reid_confidence(combined)$CONFIDENCE, c(0.5,>

===== RUN README.md:653 =====
  OK   README.md:653 +1  <reid_stability(reid_by_num, pairs, "AGE", seeds = 1:20)>

===== RUN README.md:674 =====
  ---- README.md:674 +1  (no #> expected)
  OK   README.md:674 +5  <k>
  OK   README.md:674 +10  <reid_evaluate(score_by_knowledge(pairs, k), seeds = 1:20)>

===== RUN README.md:703 =====
  OK   README.md:703 +1  <reid_knowledge_curve(   pairs,   quasi_identifiers = c(AGE =>

===== RUN README.md:734 =====
  OK   README.md:734 +1  <unicity_fraction(raw, c("AGE", "ZIP"))>
  OK   README.md:734 +4  <unicity(raw, attributes = c("AGE", "ZIP", "SEX"), p = 1:3, s>

===== RUN README.md:757 =====
  ---- README.md:757 +1  (no #> expected)
  OK   README.md:757 +3  <spatiotemporal_unicity(st, k = c(1, 2, 4), time_resolution =>

===== RUN README.md:780 =====
  OK   README.md:780 +1  <coarsen_place(c("P001", "P002", "P003", "P004"), resolution >
  OK   README.md:780 +3  <coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)>

===== RUN README.md:794 =====
  ---- README.md:794 +1  (no #> expected)
  ---- README.md:794 +3  (no #> expected)
  OK   README.md:794 +11  <names(m)>

===== RUN README.md:827 =====
  ---- README.md:827 +1  (no #> expected)
  OK   README.md:827 +3  <reid_evaluate(score_count(m_pairs), seeds = 1:10)>

===== RUN README.md:849 =====
  ---- README.md:849 +1  (no #> expected)
  ---- README.md:849 +2  (no #> expected)
SKIP  README.md:1010 (install/help instructions)

==== 27 R block(s) run, 4 skipped; 37 output unit(s) compared, 0 mismatching ====
==== 52 exported function(s), 0 missing from README ====
