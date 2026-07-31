README:  combine_scores(list(s_age, s_zip))            0.4303  (README prints 0.4303)
         combine_scores(normalize_scores(.., 'range')) 0.8417
         combine_scores(normalize_scores(.., 'rank'))  0.8417
         score_multi(c(AGE='num', ZIP='char'))         0.8417
         order swapped: combine_scores(list(zip, age)) 0.4303  <- order-independent
         combine_scores(list(age,zip), weights=c(1,26)) 0.8417  <- hand-tuned to sd ratio

match_greedy on the README's `combined` vs the normalised one:
  greedy  README   0.4350 | normalised 0.8450
  optimal README   0.6300 | normalised 0.8950  (README prints 0.630)
