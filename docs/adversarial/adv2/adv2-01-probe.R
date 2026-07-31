## adversarial line 2 -- probe 1: invariant破壊の初期探索
## run: Rscript docs/adversarial/adv2-01-probe.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 130)
say <- function(...) cat(..., "\n", sep = "")
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("A. unicity_fraction: 属性を足すと単調非減少か")
## 数学的根拠: 属性集合 S ⊂ T のとき、T で決まる同値類は S の同値類の細分。
## 細分は「ひとりだけの類」を壊せないので unicity(S) <= unicity(T) が常に成り立つ。
d <- data.frame(
  A = c("x",     "x\ry"),
  B = c("y\rz",  "z"),
  stringsAsFactors = FALSE
)
say("d$A = ", paste(encodeString(d$A), collapse = ", "))
say("d$B = ", paste(encodeString(d$B), collapse = ", "))
say("unicity_fraction(d, 'A')        = ", unicity_fraction(d, "A"))
say("unicity_fraction(d, 'B')        = ", unicity_fraction(d, "B"))
say("unicity_fraction(d, c('A','B')) = ", unicity_fraction(d, c("A", "B")))
say("VIOLATION? ", unicity_fraction(d, c("A", "B")) < unicity_fraction(d, "A"))

## 同じ壊れ方を「普通の」見た目のデータで
d2 <- data.frame(
  NAME = c("Ann", "Ann\rSmith"),
  NOTE = c("Smith\rNY", "NY"),
  stringsAsFactors = FALSE
)
say("d2: 1属性 ", unicity_fraction(d2, "NAME"), " -> 2属性 ", unicity_fraction(d2, c("NAME", "NOTE")))

## NA と文字列 "NA" の衝突
d3 <- data.frame(A = c(NA, "NA"), B = c("z", "z"), stringsAsFactors = FALSE)
say("NA vs 'NA': unicity_fraction = ", unicity_fraction(d3, c("A", "B")), " (should be 1)")

## as.character の 15 桁丸め
d4 <- data.frame(V = c(1, 1 + 1e-16))
say("double 1 vs 1+1e-16: identical? ", identical(d4$V[1], d4$V[2]),
    "  unicity_fraction = ", unicity_fraction(d4, "V"))

## ---------------------------------------------------------------------------
hdr("B. reid_evaluate: ブロック済み候補表を『総当たり』と誤認するか")
## README: 「全結合なら必ず n_anon x n_raw 行あるため」検出できる、と保証。
raw  <- data.frame(ROW_NUMBER = 1:4, ZIP = c("A", "A", "B", "B"), AGE = c(20, 30, 40, 50),
                   stringsAsFactors = FALSE)
anon <- data.frame(ROW_NUMBER = 1:4, ZIP = c("A", "A", "C", "C"), AGE = c(20, 30, 40, 50),
                   stringsAsFactors = FALSE)
cand <- block_candidates(raw, anon, keys = "ZIP")
say("blocking attr recall = ", attr(cand, "blocking")$recall)
say("candidate rows       = ", nrow(cand))
sc <- score_num(cand, "AGE")
ev <- reid_evaluate(sc, seeds = 1:5, top_k = c(1, 2))
say("evaluate$blocked           = ", ev$blocked, "   <-- should be TRUE")
say("evaluate$n_anon            = ", ev$n_anon, " (real ANON count = 4)")
say("evaluate$candidate_coverage= ", ev$candidate_coverage)
say("evaluate$success_analytic  = ", ev$success_analytic)
print(ev)
## 総当たりで測ったときの値
full <- reid_evaluate(score_num(join_raw_anon_data(raw, anon), "AGE"), seeds = 1:5, top_k = c(1, 2))
say("full-join success_analytic = ", full$success_analytic)

## ---------------------------------------------------------------------------
hdr("C. 浮動小数点: 数学的には同点なのに同点にならない")
## 数学的根拠: v1, v2 が ANON 値 a について対称 (a - v1 == v2 - a) なら
## |a-v1| == |a-v2| が厳密に成り立つので、この 2 候補は区別できず risk = 1/2。
raw  <- data.frame(ROW_NUMBER = 1:3, V = c(41.2, 42.3, 43.4))
anon <- data.frame(ROW_NUMBER = 1:3, V = c(41.2, 42.3, 43.4))
a <- 42.3
say("42.3 - 41.2 = ", sprintf("%.20g", a - 41.2))
say("43.4 - 42.3 = ", sprintf("%.20g", 43.4 - a))
say("equal in exact arithmetic? TRUE ; in doubles? ", (a - 41.2) == (43.4 - a))

## ANON 側だけ 1 人を「本人の値から外す」= 真の相手が最良でなくなる構成
raw2  <- data.frame(ROW_NUMBER = 1:3, V = c(41.2, 99.0, 43.4))
anon2 <- data.frame(ROW_NUMBER = 1:3, V = c(41.2, 42.3, 43.4))
p2 <- join_raw_anon_data(raw2, anon2)
s2 <- score_num(p2, "V")
print(as.data.frame(s2)[s2$ANON_ROW_NUMBER == 2, ])
cf <- reid_confidence(s2)
print(cf[cf$ANON_ROW_NUMBER == 2, ])
say("ANON 2 の TIE_SIZE = ", cf$TIE_SIZE[cf$ANON_ROW_NUMBER == 2], " (exact math では 2)")

## ---------------------------------------------------------------------------
hdr("D. Inf / 巨大値がスコアに入ったとき")
rawI  <- data.frame(ROW_NUMBER = 1:3, V = c(1, Inf, Inf))
anonI <- data.frame(ROW_NUMBER = 1:3, V = c(1, 2, 3))
sI <- score_num(join_raw_anon_data(rawI, anonI), "V")
print(reid_confidence(sI))
r <- tryCatch(print(reid_evaluate(sI, seeds = 1:3, top_k = 1)),
              error = function(e) say("reid_evaluate ERROR: ", conditionMessage(e)))

hdr("D2. RAW に 1 件だけ Inf -> 全レコードの ECCENTRICITY が 0 に潰れるか")
rawI2  <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, Inf))
anonI2 <- data.frame(ROW_NUMBER = 1:5, V = c(11, 19, 33, 44, 50))
sI2 <- score_num(join_raw_anon_data(rawI2, anonI2), "V")
print(reid_confidence(sI2))
