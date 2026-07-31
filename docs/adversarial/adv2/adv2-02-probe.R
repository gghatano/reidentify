## adversarial line 2 -- probe 2: 見つけた破れ方の深掘り
## run: Rscript docs/adversarial/adv2-02-probe.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
say <- function(...) cat(..., "\n", sep = "")
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("B2. blocked=FALSE なのに真ペアが欠けている（警告バナーごと消える）")
## RAW の ZIP は A,A,B,B / ANON は全員 A。ZIP でブロックすると
## 生き残るのは RAW{1,2} だけ。ANON 4 件 x RAW 2 件 = 8 行 = n_anon*n_raw
## なので reid_evaluate は「総当たり」と判定する。
raw  <- data.frame(ROW_NUMBER = 1:4, ZIP = c("A", "A", "B", "B"),
                   AGE = c(20, 30, 40, 50), stringsAsFactors = FALSE)
anon <- data.frame(ROW_NUMBER = 1:4, ZIP = c("A", "A", "A", "A"),
                   AGE = c(20, 30, 40, 50), stringsAsFactors = FALSE)
cand <- suppressWarnings(block_candidates(raw, anon, keys = "ZIP"))
b <- attr(cand, "blocking")
say("blocking: kept ", b$n_pairs_kept, " pairs, recall = ", b$recall,
    ", true pairs kept = ", b$n_true_pairs_kept, "/", b$n_true_pairs)
ev <- reid_evaluate(score_num(cand, "AGE"), seeds = 1:5, top_k = c(1, 2))
say("ev$blocked            = ", ev$blocked, "   <-- README は検出を保証している")
say("ev$candidate_coverage = ", ev$candidate_coverage)
say("ev$n_true_missing     = ", ev$n_true_missing, "  <-- 計算されているが blocked=FALSE なので印字されない")
say("ev$success_analytic   = ", ev$success_analytic)
cat("--- print() 出力（BLOCKED バナーが出ないことを確認）---\n")
print(ev)
evf <- reid_evaluate(score_num(join_raw_anon_data(raw, anon), "AGE"), seeds = 1:5, top_k = c(1, 2))
say("総当たりでの success_analytic = ", evf$success_analytic, " / blocked = ", evf$blocked)

## ---------------------------------------------------------------------------
hdr("C2. 単位を変えただけ（x10）でリスクが変わる: score_num の尺度同変性の破れ")
## 数学的根拠: score_num は |raw - anon|。exact arithmetic では
## すべての値を定数 c>0 倍すると全スコアが c 倍になるだけで、
## 同点構造・順位構造・したがって risk / TIE_SIZE / ECCENTRICITY は不変。
mk <- function(v_raw, v_anon) {
  raw  <- data.frame(ROW_NUMBER = seq_along(v_raw),  V = v_raw)
  anon <- data.frame(ROW_NUMBER = seq_along(v_anon), V = v_anon)
  score_num(join_raw_anon_data(raw, anon), "V")
}
## 3 人。ANON2 は本人 RAW2 と RAW3 のちょうど中点にいる（exact math では 2 者同点）。
v_raw_int  <- c(100, 412, 434)   # 整数表現
v_anon_int <- c(100, 423, 434)
v_raw_dec  <- v_raw_int  / 10    # 同じデータを 1/10 単位で表現
v_anon_dec <- v_anon_int / 10
s_int <- mk(v_raw_int,  v_anon_int)
s_dec <- mk(v_raw_dec,  v_anon_dec)
say("exact math: |423-412| = ", 423 - 412, " , |434-423| = ", 434 - 423, " -> 同点")
say("doubles   : |42.3-41.2| = ", sprintf("%.17g", 42.3 - 41.2),
    " , |43.4-42.3| = ", sprintf("%.17g", 43.4 - 42.3))
c_int <- reid_confidence(s_int); c_dec <- reid_confidence(s_dec)
cat("--- 整数表現 ---\n"); print(c_int)
cat("--- 1/10 表現 ---\n"); print(c_dec)
e_int <- reid_evaluate(s_int, seeds = 1:20, top_k = 1)
e_dec <- reid_evaluate(s_dec, seeds = 1:20, top_k = 1)
say("success_analytic: 整数 ", e_int$success_analytic, "  /  1/10 ", e_dec$success_analytic)
say("ANON2 の RISK   : 整数 ",
    e_int$per_record$RISK[e_int$per_record$ANON_ROW_NUMBER == 2], "  /  1/10 ",
    e_dec$per_record$RISK[e_dec$per_record$ANON_ROW_NUMBER == 2])
say("max_risk        : 整数 ", e_int$max_risk, "  /  1/10 ", e_dec$max_risk)

## 規模を上げて、系統的なズレの大きさを測る
hdr("C3. 同じ現象を n=200 で: 単位変更による success_analytic のズレ")
set.seed(11)
n <- 200
base <- sample(seq(1000, 99999, by = 1), n)          # 整数（例: 0.1 円単位の金額）
delta <- sample(c(3, 5, 7, 11), n, replace = TRUE)
raw_i  <- base
anon_i <- base + delta
## ペアごとに「対称な第 3 者」を仕込む: RAW に anon_i + delta を持つ人を作る
raw_i  <- c(raw_i, anon_i + delta)
row_raw <- data.frame(ROW_NUMBER = seq_along(raw_i), V = raw_i)
row_an  <- data.frame(ROW_NUMBER = seq_len(n),       V = anon_i)
s_i <- score_num(join_raw_anon_data(row_raw, row_an), "V")
row_raw_d <- data.frame(ROW_NUMBER = seq_along(raw_i), V = raw_i / 10)
row_an_d  <- data.frame(ROW_NUMBER = seq_len(n),       V = anon_i / 10)
s_d <- score_num(join_raw_anon_data(row_raw_d, row_an_d), "V")
ei <- reid_evaluate(s_i, seeds = 1:20, top_k = c(1, 5))
ed <- reid_evaluate(s_d, seeds = 1:20, top_k = c(1, 5))
say("整数表現 : success_analytic ", sprintf("%.6f", ei$success_analytic),
    "  max_risk ", ei$max_risk, "  TIE_SIZE==2 の件数 ", sum(ei$per_record$TIE_SIZE == 2))
say("1/10 表現: success_analytic ", sprintf("%.6f", ed$success_analytic),
    "  max_risk ", ed$max_risk, "  TIE_SIZE==2 の件数 ", sum(ed$per_record$TIE_SIZE == 2))
say("差 = ", sprintf("%.6f", ed$success_analytic - ei$success_analytic))
say("RISK が食い違うレコード数 = ",
    sum(ei$per_record$RISK[order(ei$per_record$ANON_ROW_NUMBER)] !=
        ed$per_record$RISK[order(ed$per_record$ANON_ROW_NUMBER)]))

## ---------------------------------------------------------------------------
hdr("D3. Inf: reid_evaluate の返り値契約が壊れ、print() が例外を投げる")
rawI  <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, Inf))
anonI <- data.frame(ROW_NUMBER = 1:5, V = c(11, 19, 33, 44, 50))
sI <- score_num(join_raw_anon_data(rawI, anonI), "V")
evI <- reid_evaluate(sI, seeds = 1:5, top_k = 1)
say("class(precision_recall) = ", paste(class(evI$precision_recall), collapse = "/"),
    "  (documented: data frame)")
say("is.null(precision_recall) = ", is.null(evI$precision_recall))
say("per_record$CONFIDENCE = ", paste(evI$per_record$CONFIDENCE, collapse = ", "))
r <- tryCatch({ print(evI); "no error" },
              error = function(e) paste("print() ERROR:", conditionMessage(e)))
say(r)
say("match_greedy CONFIDENCE = ",
    paste(match_greedy(sI, seed = 1)$CONFIDENCE, collapse = ", "))
r2 <- tryCatch({ match_greedy(sI, seed = 1, min_confidence = 0.5); "no error" },
               error = function(e) paste("match_greedy(min_confidence) ERROR:",
                                         conditionMessage(e)))
say(r2)
