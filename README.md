# reidentify

匿名加工データの**再識別リスクを測るための R パッケージ**です。

匿名加工を施したデータ（ANON）に対して、攻撃者が手元に持っていると想定する
データ（RAW）を突き合わせ、**どれだけの割合のレコードが元の個人に紐づけ直せて
しまうか**を推定します。攻撃の実演ではなく、「この加工でどれだけ守れているか」を
数値で示すことが目的です。

公開ページ: <https://gghatano.github.io/reidentify/>

> [!IMPORTANT]
> **研究・検討用のツールです。** 出力される数値は、法令適合性や「安全である」ことを
> 保証するものではありません。詳しくは [限界と注意](#限界と注意) を必ずお読みください。

---

## 目次

- [インストール](#インストール)
- [30 秒でわかる使い方](#30-秒でわかる使い方)
- [考え方: 3 層 API](#考え方-3-層-api)
- [多属性を同時に使う](#多属性を同時に使う)
- [まれな値ほど強い手がかり: IDF 重み](#まれな値ほど強い手がかり-idf-重み)
- [集合属性（買い物かご・訪問先）](#集合属性買い物かご訪問先)
- [大規模データ: ブロッキングで候補を絞る](#大規模データ-ブロッキングで候補を絞る)
- [一般化された値（「30代」「東京都」）](#一般化された値30代東京都)
- [疎なデータ: Scoreboard-RH](#疎なデータ-scoreboard-rh)
- [割当層: 貪欲と大域最適](#割当層-貪欲と大域最適)
- [信頼度 `CONFIDENCE`](#信頼度-confidence)
- [評価指標: `reid_evaluate()`](#評価指標-reid_evaluate)
- [攻撃者知識モデル (W / M / S)](#攻撃者知識モデル-w--m--s)
- [ユニシティ](#ユニシティ)
- [トランザクションデータからマスタ形式へ](#トランザクションデータからマスタ形式へ)
- [関数一覧](#関数一覧)
- [限界と注意](#限界と注意)
- [ドキュメント](#ドキュメント)

---

## インストール

R 4.x と、依存パッケージ（`dplyr`, `magrittr`, `stringi`, `tibble`, `philentropy`）が
必要です。**`devtools` は不要**です。

`match_optimal()`（大域最適割当）を使う場合は `clue` も必要です。
無くても、他の機能はそのまま動きます。
`score_minhash()` / `lsh_candidates()` のハッシュは自前実装なので、追加の依存はありません。

### 方法 1: GitHub から直接（base R のみ、1 行）

```r
install.packages(c("dplyr", "magrittr", "stringi", "tibble", "philentropy"))
install.packages("clue")  # match_optimal()（大域最適割当）を使う場合

install.packages(
  "https://github.com/gghatano/reidentify/archive/refs/heads/master.tar.gz",
  repos = NULL, type = "source"
)
```

`untar2` の `skipping pax global extended headers` という警告が出ますが、
GitHub の tar 形式によるもので、インストール自体には影響しません。

### 方法 2: clone して `R CMD INSTALL`

```sh
git clone https://github.com/gghatano/reidentify.git
cd reidentify
R CMD INSTALL .
```

### 方法 3: 開発時（インストールせずに読み込む）

パッケージを編集しながら使う場合は `pkgload` を使います。**インストール済みの
古いコピーを読んでしまう事故を避けられる**ので、開発中はこちらを推奨します。

```r
# install.packages("pkgload")
pkgload::load_all("path/to/reidentify")
```

`devtools` があれば `devtools::install_github("gghatano/reidentify")` も使えますが、
上記のいずれも `devtools` なしで動きます。

### テストの実行

```r
# install.packages("testthat")
pkgload::load_all(".")
testthat::test_dir("tests/testthat")
```

---

## 30 秒でわかる使い方

以下はコピー＆ペーストでそのまま動きます。`#>` で始まる行は実行結果そのままです
（このページの全コード例は 1 セッションで通し実行し、出力を機械照合しています）。

```r
library(reidentify)

## RAW  = 攻撃者が既に持っていると想定するデータ
## ANON = 公開された匿名加工データ
## ここではダミーデータを使い、ANON 側の AGE にだけノイズを加えて
## 「年齢をぼかす加工」を模擬する
raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)

set.seed(42)  # ノイズの付け方も乱数なので、再現のため固定する
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)

head(raw)
#> # A tibble: 6 × 9
#>   ROW_NUMBER    ID   AGE ZIP   SEX   VISIT_COUNT SPEND_MEAN SPEND_DIST          
#>        <int> <int> <int> <chr> <chr>       <int>      <dbl> <chr>               
#> 1          1 10001    54 Z036  F               4       35.5 81:7:40:14          
#> 2          2 10002    41 Z007  F               7       42.1 19:84:72:27:50:8:35 
#> 3          3 10003    24 Z034  M               1       97   97                  
#> 4          4 10004    21 Z017  M               2       64   62:66               
#> 5          5 10005    23 Z023  M              11       67.5 31:41:100:86:95:81:…
#> 6          6 10006    79 Z028  F              14       48.9 40:81:8:36:44:16:58…
#> # ℹ 1 more variable: FINGERPRINT <dbl>

## RAW と ANON は ROW_NUMBER で対応づけられており、
## 同じ ROW_NUMBER が同じ人物であることが「正解」として使われます

## RAW x ANON の全候補ペアを作る（列に RAW_ / ANON_ の接頭辞が付く）
pairs <- join_raw_anon_data(raw, anon)

## AGE の近さだけを手がかりに再識別を試み、結果を評価する
scores <- score_num(pairs, "AGE")
reid_evaluate(scores, seeds = 1:20)
#> reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
#>   success rate   : 0.1127 exact | simulated mean 0.1108 sd 0.0221 range [0.0800, 0.1550] over 20 seeds
#>   baseline       : random 0.0050 | mode 0.0050   (lift vs random: 22.54x)
#>   top-k hit rate : k=1 0.1127  k=5 0.3728  k=10 0.5363
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 0.1164 : attack 1/200 (0.5%)  precision 0.0000  recall 0.0000
#>     conf >= 0.1143 : attack 7/200 (3.5%)  precision 0.0000  recall 0.0000
#>     conf >= 0.1114 : attack 8/200 (4.0%)  precision 0.0000  recall 0.0000
#>     conf >= 0.1010 : attack 12/200 (6.0%)  precision 0.0000  recall 0.0000
#>     conf >= 0.0741 : attack 16/200 (8.0%)  precision 0.0625  recall 0.0050
#>     ... 4 more threshold(s)
```

**読み方**: 年齢だけを使った攻撃では 11.3% しか当たりません。ただしランダム割当の
ベースラインが 0.5% なので、**それでも 22.5 倍**です。さらに `max per-record risk`
が 1.0000 ＝ **確実に特定されるレコードが存在する**ことを示しています。
平均だけを見ていると見落とす情報です。

---

## 考え方: 3 層 API

再識別攻撃を **「スコア層 → 統合層 → 割当層」** の 3 段に分けています。
属性ごとの距離の測り方と、候補の選び方を分離することで、
「属性 N 個 × 距離定義 × 割当方式」を組み合わせて回せます。

```text
[1] スコア層   score_num() / score_char() / score_jaccard() / ...
                  → (RAW_ROW_NUMBER, ANON_ROW_NUMBER, SCORE) のロング形式

[2] 統合層     normalize_scores() → combine_scores(list(...), weights = ...)
                  → 複数属性のスコアを 1 本にまとめる

[3] 割当層     match_greedy() / match_optimal() / match_scoreboard_rh()
                  → (ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT)
```

| 層 | 関数 | 役割 |
|---|---|---|
| スコア層（基本） | `score_num()` | 数値属性の距離 |
| | `score_char()` | 文字列属性の一致度 |
| | `score_dist()` | 分布列（`"1:2:3"` 形式）の分布間距離 |
| | `score_num_rank()` | 数値の順位の近さ |
| スコア層（多属性） | `score_multi()` | 複数列を宣言して 1 本のスコアにする |
| | `score_mahalanobis()` | 相関のある数値列の冗長さを打ち消す距離 |
| スコア層（頻度重み） | `score_idf()` / `score_idf_match()` | まれな値の一致を強く数える |
| スコア層（集合） | `score_jaccard()` / `score_minhash()` | 集合属性の重なり |
| スコア層（行動） | `score_count()` / `score_span()` / `score_profile()` | 件数・時間幅・構成比 |
| スコア層（一般化） | `score_containment()` | 「30代」に入りうる RAW を絞る |
| スコア層（疎データ） | `score_scoreboard()` | 欠測だらけの疎行列向け |
| 統合層 | `normalize_scores()` | 尺度の違う複数スコアを共通尺度に載せる |
| | `combine_scores()` | 正規化して加重和 |
| 軸の診断 | `axis_informativeness()` / `axis_report()` | 各軸が本当に情報を持つかの検定 |
| 割当層 | `match_greedy()` | ANON 1 件ごとに最良の RAW を選ぶ |
| | `match_optimal()` | 全体の総コストが最小になる 1 対 1 割当 |
| | `match_scoreboard_rh()` | 確信度の閾値 φ を満たさない候補を棄却する割当 |

複数属性を組み合わせる例:

```r
s_age <- score_num(pairs, "AGE")
s_zip <- score_char(pairs, "ZIP")

combined <- combine_scores(list(s_age, s_zip))
picked   <- match_greedy(combined, seed = 1)

head(picked, 5)
#>   ANON_ROW_NUMBER RAW_ROW_NUMBER CONFIDENCE RESULT
#> 1               1            121 0.11578124  FALSE
#> 2               2              2 0.00000000   TRUE
#> 3               3            166 0.00000000  FALSE
#> 4               4              4 0.05924507   TRUE
#> 5               5              5 0.06046164   TRUE

reid_evaluate(combined, seeds = 1:20)
#> reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
#>   success rate   : 0.4303 exact | simulated mean 0.4363 sd 0.0121 range [0.4100, 0.4600] over 20 seeds
#>   baseline       : random 0.0050 | mode 0.0050   (lift vs random: 86.06x)
#>   top-k hit rate : k=1 0.4303  k=5 0.8387  k=10 0.9940
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 0.2364 : attack 1/200 (0.5%)  precision 1.0000  recall 0.0050
#>     conf >= 0.2080 : attack 2/200 (1.0%)  precision 1.0000  recall 0.0100
#>     conf >= 0.1945 : attack 3/200 (1.5%)  precision 1.0000  recall 0.0150
#>     conf >= 0.1796 : attack 4/200 (2.0%)  precision 1.0000  recall 0.0200
#>     conf >= 0.1791 : attack 5/200 (2.5%)  precision 1.0000  recall 0.0250
#>     ... 109 more threshold(s)
```

AGE 単独の 11.3% が、ZIP を足しただけで 43.0% に上がります。

1 行目に注目してください。`CONFIDENCE` が 5 件中で最も高い（＝攻撃者から見れば
最も確信できるケース）にもかかわらず `RESULT` は `FALSE` です。
**確信度の高さは正しさを意味しません。**

`CONFIDENCE` の定義と、その閾値をどう選ぶかは
[信頼度 `CONFIDENCE`](#信頼度-confidence) を参照してください。

### 従来の 1 発呼び出し API

3 層に分ける前からある `reid_by_*()` も残っています。内部では上記の
スコア層＋割当層を呼んでいます。

```r
r <- reid_by_num(pairs, "AGE", seed = 1)

## NB: reid_result() は値を *invisible* で返すため、明示的に print() が要ります
print(reid_result(r, method = "AGE"))
#> [1] " method: AGE , success / trial :  23 / 200"
```

| 関数 | 対応するスコア層 |
|---|---|
| `reid_by_num()` | `score_num()` |
| `reid_by_char()` | `score_char()` |
| `reid_by_dist()` | `score_dist()` |
| `reid_by_num_rank()` | `score_num_rank()` |

`reid_result()` は「成功数 / 試行数」しか返しません。**ベースラインとの比較も
ばらつきも出ないため、新規に書くコードでは `reid_evaluate()` を推奨します。**

---

## 多属性を同時に使う

現実の攻撃者は、手元にある列を 1 つずつではなく**同時に**使います。
`score_multi()` は列と型を宣言するだけで、正規化・統合まで面倒を見ます。

```r
s_multi <- score_multi(pairs, c(AGE = "num", ZIP = "char", SEX = "char"))
reid_evaluate(s_multi, seeds = 1:20)
#> reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
#>   success rate   : 0.9200 exact | simulated mean 0.9203 sd 0.0068 range [0.9100, 0.9300] over 20 seeds
#>   baseline       : random 0.0050 | mode 0.0050   (lift vs random: 184.00x)
#>   top-k hit rate : k=1 0.9200  k=5 1.0000  k=10 1.0000
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 1.0279 : attack 1/200 (0.5%)  precision 1.0000  recall 0.0050
#>     conf >= 0.9199 : attack 2/200 (1.0%)  precision 1.0000  recall 0.0100
#>     conf >= 0.9023 : attack 3/200 (1.5%)  precision 1.0000  recall 0.0150
#>     conf >= 0.8970 : attack 4/200 (2.0%)  precision 1.0000  recall 0.0200
#>     conf >= 0.8813 : attack 5/200 (2.5%)  precision 1.0000  recall 0.0250
#>     ... 187 more threshold(s)
```

### 足した軸が本当に効いているかを確かめる

**無情報な軸を等重みで足すと、単一属性だけのときより成功率が下がることがあります**
（Issue #35 の実測）。手がかりを増やしたのに数値が下がるので、
下がった数値を疑いにくいという点で危険です。

`axis_report()` は各軸を単独で評価し、ランダム割当より有意に良いかを検定します。
`score_multi()` は既定（`screen = "warn"`）でこれを内部でも走らせ、
無情報な軸があれば警告します（`screen = "drop"` で自動除外、`"none"` で無効化）。

```r
axis_report(s_multi)
#> axis informativeness (3 axis/axes, alpha = 0.05)
#>   AGE                  success 0.1127  baseline 0.0050  lift  22.54x  rank 0.048  z  22.27  p = 0.0000  informative
#>   ZIP                  success 0.2000  baseline 0.0050  lift  40.00x  rank 0.017  z  29.66  p = 0.0000  informative
#>   SEX                  success 0.0100  baseline 0.0050  lift   2.00x  rank 0.253  z  14.14  p = 0.0000  informative
```

この例では SEX の lift は 2.00 倍しかありません。有意ではあるものの、
**ZIP（40 倍）と等しい重みで足してよい軸ではない**ことがこの表から読めます。
重みは `score_multi(..., weights = )` で指定できます。

相関の強い数値列（身長と体重、購入回数と購入金額など）を等重みで足すと、
実質 1 列分の情報を 2 列分として数えてしまいます。`score_mahalanobis()` は
RAW 側の共分散でこの冗長さを打ち消します。

```r
head(score_mahalanobis(pairs, c("AGE", "VISIT_COUNT")), 3)
#> reid scores (distance): 3 candidate pair(s), 1 ANON x 3 RAW record(s)
#>   RAW_ROW_NUMBER ANON_ROW_NUMBER     SCORE
#> 1              1               1 0.1191868
#> 2              2               1 0.8720191
#> 3              3               1 1.7072173
```

尺度合わせだけを手動で行いたい場合は `normalize_scores()` を使います
（`"range"` / `"zscore"` / `"rank"` / `"none"`）。

---

## まれな値ほど強い手がかり: IDF 重み

「東京都在住」は誰でも当てはまりますが、「特定の郵便番号が一致」はほぼ本人です。
`score_idf()` はこの差を、値の出現頻度の逆数（IDF）で重み付けします。

```r
head(value_frequencies(pairs, "ZIP"), 3)
#>   VALUE COUNT SHARE
#> 1  Z013     9 0.045
#> 2  Z017     9 0.045
#> 3  Z021     9 0.045

reid_evaluate(score_idf_match(pairs, c("ZIP", "SEX")), seeds = 1:20)
#> reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
#>   success rate   : 0.3750 exact | simulated mean 0.3728 sd 0.0310 range [0.3150, 0.4350] over 20 seeds
#>   baseline       : random 0.0050 | mode 0.0050   (lift vs random: 75.00x)
#>   top-k hit rate : k=1 0.3750  k=5 0.9650  k=10 1.0000
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 10.2338 : attack 1/200 (0.5%)  precision 1.0000  recall 0.0050
#>     conf >= 1.2347 : attack 3/200 (1.5%)  precision 1.0000  recall 0.0150
#>     conf >= 1.1747 : attack 6/200 (3.0%)  precision 1.0000  recall 0.0300
#>     conf >= 1.1719 : attack 8/200 (4.0%)  precision 1.0000  recall 0.0400
#>     conf >= 1.1354 : attack 12/200 (6.0%)  precision 1.0000  recall 0.0600
#>     ... 6 more threshold(s)
```

**IDF は「必ず強くなる手法」ではありません。** このダミーデータの ZIP は
値がほぼ一様分布なので、まれな値と平凡な値の差がなく、単独では何も変わりません。

```r
c(char = reid_evaluate(score_char(pairs, "ZIP"), seeds = 1:20)$success_analytic,
  idf  = reid_evaluate(score_idf(pairs, "ZIP"), seeds = 1:20)$success_analytic)
#> char  idf 
#>  0.2  0.2 
```

IDF が効くのは値の分布が偏っているとき（実データの商品コード、訪問先など）です。
効いているかどうかは、こうして**両方測って比べる**以外に確かめようがありません。

---

## 集合属性（買い物かご・訪問先）

「買った商品の集合」「訪れた店の集合」のような**集合値の列**は、
分布距離で測ると実際のリスクを大幅に取り逃がします。

```r
## 200 人が 500 品目のカタログから 8 品目を買い、公開時は半分が抑止される
set.seed(20260731)
pop    <- 1 / seq_len(500)^1.1
basket <- lapply(1:200, function(i) sort(sample.int(500, 8, prob = pop)))
kept   <- lapply(basket, function(v) sort(sample(v, 4)))

set_raw  <- data.frame(ROW_NUMBER = 1:200,
                       ITEMS = vapply(basket, paste, character(1), collapse = ":"))
set_anon <- data.frame(ROW_NUMBER = 1:200,
                       ITEMS = vapply(kept, paste, character(1), collapse = ":"))
set_pairs <- join_raw_anon_data(set_raw, set_anon)

head(set_raw, 2)
#>   ROW_NUMBER               ITEMS
#> 1          1  1:2:4:5:9:12:61:94
#> 2          2 1:2:4:8:11:15:20:55
head(set_anon, 2)
#>   ROW_NUMBER      ITEMS
#> 1          1   1:2:4:94
#> 2          2 1:11:20:55

c(dist    = reid_evaluate(score_dist(set_pairs, "ITEMS"), seeds = 1:20)$success_analytic,
  jaccard = reid_evaluate(score_jaccard(set_pairs, "ITEMS"), seeds = 1:20)$success_analytic,
  minhash = reid_evaluate(score_minhash(set_pairs, "ITEMS"), seeds = 1:20)$success_analytic)
#>      dist   jaccard   minhash 
#> 0.0700000 0.9446667 0.9275000 
```

**同じデータ・同じ攻撃者で、測り方を変えただけで 13 倍以上違います。**
分布距離は集合を「値のヒストグラム」として扱うため、
「この稀な品目を両方が持っている」という最も強い証拠を捨ててしまいます。
`score_jaccard()` は `method = "dice"` / `"overlap"` / `"tversky"` も選べます。

件数が多くて総当たりが重いときは、次節の `lsh_candidates()` で候補を絞れます。

---

## 大規模データ: ブロッキングで候補を絞る

`join_raw_anon_data()` は RAW × ANON の**全**ペアを作ります。これは n² で伸びるため、
n = 100,000 では 10¹⁰ ペア・約 149 GB になります。**どんなに速い割当ソルバでも
この壁は越えられません**（Issue #36）。

ブロッキングは「安いキーが一致するペアだけを候補にする」ことで n² を崩します。
`block_candidates()` は準識別子の完全一致（またはその粗視化）で絞ります。

```r
cand <- block_candidates(raw, anon, keys = "ZIP")
attr(cand, "blocking")
#> blocking (deterministic): 1,156 of 40,000 pair(s) kept (2.89% of the full 200 x 200 join)
#>   recall       : 1.0000  (200 of 200 true pair(s) retained)
#>   ANON records with no candidate at all: 0
#>   settings     : keys = ZIP
```

ペアの 97.1% を捨てて、**正解ペアは 1 つも失っていません**（recall 1.0000）。
このとき再識別率は総当たりと完全に一致します。

```r
qi <- c(AGE = "num", ZIP = "char", SEX = "char")
c(full    = reid_evaluate(score_multi(pairs, qi, screen = "none"),
                          seeds = 1:20)$success_analytic,
  blocked = reid_evaluate(score_multi(cand,  qi, screen = "none"),
                          seeds = 1:20)$success_analytic)
#>    full blocked 
#>    0.92    0.92 
```

（`screen = "none"` を渡しているのは、ブロッキングキーは**ブロック内では定数**に
なるため、同じ列をスコアにも入れると軸診断が正しく「無情報」と警告するからです。）

### 再現率（recall）を必ず見る

**ブロッキングが正解ペアを取りこぼすと、再識別率は実際より低く出ます。**
低い数値は歓迎され、疑われにくいので、これはもっとも危険な壊れ方です。
この節の関数はすべて **recall（正解ペアが候補に残った割合）を実測して報告し、
1 を下回れば警告します。**

ANON 側で AGE にノイズを加えてあるので、AGE の完全一致でブロックすると壊れます。

```r
lossy <- block_candidates(raw, anon, keys = "AGE")
attr(lossy, "blocking")
#> blocking (deterministic): 768 of 40,000 pair(s) kept (1.92% of the full 200 x 200 join)
#>   recall       : 0.3900  (78 of 200 true pair(s) retained)
#>   ANON records with no candidate at all: 2
#>   ! 122 true pair(s) were discarded. A reidentification rate measured on
#>     this candidate set is a LOWER bound: those records cannot be found.
#>   settings     : keys = AGE
```

`reid_evaluate()` は、渡された候補表が総当たりでないことを**スコア表そのものから
検出します**（全結合なら必ず `n_anon × n_raw` 行あるため）。属性を引き継ぐ必要も、
利用者が申告する必要もありません。

```r
reid_evaluate(score_multi(lossy, qi, screen = "none"), seeds = 1:20)
#> reid evaluation: 198 ANON x 191 RAW record(s), 768 candidate pair(s)
#>   candidate set  : BLOCKED -- 2.031% of the full 37818-pair join kept
#>     true RAW record absent from the candidates of 120/198 ANON record(s)
#>     -> the success rate below is a LOWER bound. ANON records that were left
#>        with no candidate at all are not counted here at all.
#>   success rate   : 0.3838 exact | simulated mean 0.3833 sd 0.0049 range [0.3737, 0.3889] over 20 seeds
#>   baseline       : random 0.1122 | mode 0.0051   (lift vs random: 3.42x)
#>   top-k hit rate : k=1 0.3838  k=5 0.3939
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= Inf : attack 21/198 (10.6%)  precision 0.0952  recall 0.0101
#>     conf >= 2.2361 : attack 23/198 (11.6%)  precision 0.1304  recall 0.0152
#>     conf >= 2.0000 : attack 31/198 (15.7%)  precision 0.1613  recall 0.0253
#>     conf >= 1.7321 : attack 41/198 (20.7%)  precision 0.1951  recall 0.0404
#>     conf >= 1.6641 : attack 43/198 (21.7%)  precision 0.2093  recall 0.0455
#>     ... 42 more threshold(s)
```

0.92 が 0.38 に落ちています。**キーの選び方だけで、リスクが 2.4 倍安全に見えます。**

取りこぼしを買い戻す方法は 2 つあります。キーを粗くする（`transform`）か、
複数のキーで絞った結果を**和集合**にする（`keys` にリストを渡す）かです。

```r
attr(block_candidates(raw, anon, keys = "AGE",
                      transform = list(AGE = function(x) x %/% 10)),
     "blocking")$recall
#> [1] 0.88

attr(block_candidates(raw, anon, keys = list("ZIP", "AGE")), "blocking")$recall
#> [1] 1
```

### 集合属性には min-hash + LSH

集合値の列（買い物かご・訪問先）には一致するキーがないので、min-hash 署名の
バンド衝突で候補を作ります。`bands` を増やすほど候補は増え、recall も上がります。

```r
blocked <- lsh_candidates(set_raw, set_anon, "ITEMS", bands = 32, seed = 1)
attr(blocked, "blocking")
#> blocking (minhash-lsh): 1,355 of 40,000 pair(s) kept (3.388% of the full 200 x 200 join)
#>   recall       : 0.8850  (177 of 200 true pair(s) retained)
#>   ANON records with no candidate at all: 13
#>   ! 23 true pair(s) were discarded. A reidentification rate measured on
#>     this candidate set is a LOWER bound: those records cannot be found.
#>   settings     : n_hash = 128, bands = 32
```

**この recall 0.8850 は、Issue #36 まで見えていませんでした。** 既定の `bands = 32`
は 11.5% の正解ペアを捨てます。`bands = 64` にすると recall 1.0 になる代わりに
候補は 45% まで戻ります（実測は
[`docs/investigation/blocking-benchmark-log.txt`](docs/investigation/blocking-benchmark-log.txt)）。

### 上位 k 件だけ残す

スコアを計算した**後**で絞るのが `top_k_candidates()` です。第 1 段の安いスコアで
候補を k 件に落とし、第 2 段の高いスコアをその上だけで回す、という使い方をします。

```r
attr(top_k_candidates(s_multi, k = 10), "blocking")
#> blocking (top-k): 2,077 of 40,000 pair(s) kept (5.192% of the full 200 x 200 join)
#>   recall       : 1.0000  (200 of 200 true pair(s) retained)
#>   ANON records with no candidate at all: 0
#>   settings     : k = 10, ties = keep
```

同点は既定で**切らずに残します**（`ties = "keep"`）。k 番目と k+1 番目が同点なら
選ぶ根拠がなく、行順で切れば理由なく正解ペアを落として数値を下げるからです。

自作のフィルタで候補を絞った場合は、`blocking_recall()` で同じ指標を測れます。

```r
blocking_recall(cand, raw, anon)$kept_fraction
#> [1] 0.0289
```

### 実測: n を増やしたときの伸び方

`docs/investigation/blocking-benchmark.R` の実測（`ZIP` でブロッキング、recall は
全 n で 1.0000）。log-log 傾きで、ペア数・メモリの指数が **2.00 → 1.00** に落ちます。

| n | 総当たりペア | 総当たり秒 | 総当たり MB | ブロック後ペア | ブロック秒 | ブロック MB |
|---|---|---|---|---|---|---|
| 500 | 250,000 | 0.27 | 26.8 | 2,982 | 0.00 | 0.4 |
| 1,000 | 1,000,000 | 1.32 | 107.0 | 6,164 | 0.00 | 0.8 |
| 2,000 | 4,000,000 | 6.41 | 427.6 | 12,120 | 0.01 | 1.7 |
| 4,000 | 16,000,000 | 28.00 | 1,709.7 | 23,790 | 0.01 | 3.3 |
| 8,000 | — | — | — | 47,796 | 0.03 | 6.6 |
| 16,000 | — | — | — | 95,918 | 0.11 | 13.1 |
| 32,000 | — | — | — | 192,480 | 0.39 | 26.3 |

n = 32,000 の候補生成からスコア・評価まで通しで **49.9 秒 / 26 MB** です
（うち候補生成は 0.4 秒で、残りはスコア計算と 5 シードの評価）。
同じ問題の総当たりは 10 億ペアで、このマシンでは作れません。

---

## 一般化された値（「30代」「東京都」）

公開データで年齢が `"[30,40)"`、住所が `"東京都"` のように**粗くされている**場合、
RAW の `37` と ANON の `"[30,40)"` は「文字列として違う」のではなく
「**含まれている**」関係です。文字列距離で測るのは誤りで、
**実リスクの約 1/4 しか報告されません**（Issue #40 の実測: 0.1017 対 0.4450）。

一般化階層は CSV / YAML から読み込めます（同梱の例は `inst/extdata/`）。

```r
h <- read_generalization_hierarchy(
  system.file("extdata", "generalization-jp.csv", package = "reidentify")
)
h
#> generalization hierarchy: 50 edge(s) over 2 attribute(s)
#>   AREA: 30 edge(s), 3 level(s), root(s): 関東, 近畿, 中部
#>   AGE: 20 edge(s), 2 level(s), root(s): [0,10), [10,20), [20,30), [30,40), [40,50)

generalize_value(c(31, 37, 46), "AGE", h, levels = 1)
#> [1] "[30,40)" "[30,40)" "[40,50)"
```

```r
areas <- c("千代田区", "港区", "新宿区", "横浜市", "川崎市", "大阪市")
g_raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 24, 33, 37, 38, 52),
                    AREA = areas, stringsAsFactors = FALSE)
g_anon <- data.frame(ROW_NUMBER = 1:6,
                     AGE  = generalize_value(g_raw$AGE, "AGE", h, levels = 1),
                     AREA = generalize_value(g_raw$AREA, "AREA", h, levels = 1),
                     stringsAsFactors = FALSE)
g_anon
#>   ROW_NUMBER     AGE     AREA
#> 1          1 [20,30)   東京都
#> 2          2 [20,30)   東京都
#> 3          3 [30,40)   東京都
#> 4          4 [30,40) 神奈川県
#> 5          5 [30,40) 神奈川県
#> 6          6 [50,60)   大阪府

g_pairs <- join_raw_anon_data(g_raw, g_anon)

## 公開された各区画に何人の RAW が入りうるか（= k 匿名性の実測）
containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)
#>   ANON_ROW_NUMBER N_CANDIDATES N_CONTAINED NARROWED_TO INFORMATION
#> 1               1            6           2   0.3333333         0.5
#> 2               2            6           2   0.3333333         0.5
#> 3               3            6           1   0.1666667         1.0
#> 4               4            6           2   0.3333333         0.5
#> 5               5            6           2   0.3333333         0.5
#> 6               6            6           1   0.1666667         1.0
#>   TRUTH_CONTAINED
#> 1            TRUE
#> 2            TRUE
#> 3            TRUE
#> 4            TRUE
#> 5            TRUE
#> 6            TRUE

match_greedy(score_containment(g_pairs, c("AGE", "AREA"), hierarchy = h), seed = 1)
#>   ANON_ROW_NUMBER RAW_ROW_NUMBER CONFIDENCE RESULT
#> 1               1              1    0.00000   TRUE
#> 2               2              2    0.00000   TRUE
#> 3               3              3    2.44949   TRUE
#> 4               4              5    0.00000  FALSE
#> 5               5              5    0.00000   TRUE
#> 6               6              6    2.44949   TRUE
```

`score_char()` / `score_num()` / `score_num_rank()` は、ANON 側が一般化されている
ことを検出して**停止します**（Issue #40）。黙って小さい数値を返すほうが危険だからです。

```r
tryCatch(score_char(g_pairs, "AGE"),
         error = function(e) cat(substr(conditionMessage(e), 1, 78), "...\n"))
#> score_char(): column "AGE" is generalised on the ANON side (100% of its publis ...
```

値が一般化されているかどうかの判定は `is_generalized_value()`、
「37歳」「135****」のような単位付き・マスク済みの表記に使う単位辞書は
`generalization_units()` で確認できます。

```r
is_generalized_value(c("37", "30s", "[30,40)", "135****", "M", NA))
#> [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE
```

> **カテゴリ的な一般化は構造的に検出できません。** 「千代田区 → 東京都」は、
> 文字列 `"東京都"` を見ても `"千代田区"` を含むとは分かりません。
> したがって `score_char("AREA")` は停止せず、低い数値を黙って返します。
> **階層を宣言して `score_containment()` を使うのは利用者の責任です。**
> また、生の値と一般化値が混在し、一般化値の比率が 20% 未満の列も検出されません。

---

## 疎なデータ: Scoreboard-RH

商品評価や視聴履歴のように、**列が非常に多く、ほとんどが欠測**のデータでは、
「両方が値を持っている少数の列がどれだけ一致するか」だけが手がかりになります。
`score_scoreboard()` は Narayanan & Shmatikov (2008) の Scoreboard-RH に沿って
まれな列の一致を重く数え、`match_scoreboard_rh()` は確信度が閾値 φ に満たない
候補を**棄却**します（当てずっぽうを出さない）。棄却された ANON レコードは
`RAW_ROW_NUMBER = NA` で行としては残るので、試行数は減りません。

```r
sb_anon <- data.frame(ROW_NUMBER = 1:3,
                      I1 = c(5, NA, 1), I2 = c(NA, 2, 2),
                      I3 = c(3, 4, NA), I4 = c(NA, 1, 5))
sb_aux <- sb_anon
sb_aux$I3 <- NA          # 攻撃者は I3 を知らない
sb_pairs <- join_raw_anon_data(sb_aux, sb_anon)

match_scoreboard_rh(score_scoreboard(sb_pairs, c("I1", "I2", "I3", "I4"),
                                     tolerance = 1))
#>   ANON_ROW_NUMBER RAW_ROW_NUMBER CONFIDENCE RESULT
#> 1               1              1   1.732051   TRUE
#> 2               2              2   1.000000   TRUE
#> 3               3              3   1.309307   TRUE
```

既定は `phi = 0`（棄却しない）です。**原論文の φ = 1.5 をそのまま持ち込まないで
ください。** 閾値の尺度はスコア表の性質で決まり、リスクの大きさでは決まりません
（[次節](#信頼度-confidence)）。このパッケージのフィクスチャには、
どのレコードも 1.5 に届かないものがあります。そこで `phi = 1.5` を使うと
再識別 0 件が返り、**本当に安全なデータと区別がつきません**
（全件棄却時には警告が出ますが、それが閾値を選ぶ作業の代わりにはなりません）。

> Narayanan, A. and Shmatikov, V. (2008) Robust De-anonymization of Large
> Sparse Datasets. *IEEE Symposium on Security and Privacy*, 111–125.

---

## 割当層: 貪欲と大域最適

`match_greedy()` は ANON 1 件ずつ独立に最良の RAW を選ぶため、
同じ RAW が複数の ANON に割り当たります。`match_optimal()` は
「1 人は 1 人にしか対応しない」という制約を入れて総コストを最小化します
（ハンガリアン法 / `clue`）。

```r
c(greedy  = mean(match_greedy(combined, seed = 1)$RESULT),
  optimal = mean(match_optimal(combined, seed = 1)$RESULT))
#>  greedy optimal 
#>   0.435   0.630 
```

この例では RAW と ANON が完全に 1 対 1 対応しているため、制約が正しく効いて
成功率が上がります。

> **重なりが部分的なとき、`match_optimal()` はリスクを過小報告します**
> （Issue #15 の実測。RAW 150 件のうち 120 件だけが ANON に含まれる条件で、
> 最適 0.357 に対し貪欲 0.437。ダミー行を入れても 0.401 止まり）。
> 1 対 1 制約は「相手が必ず居る」という仮定なので、居ない相手にも
> 無理やり誰かを割り当ててしまい、正解のペアを押しのけます。
> **重なりが不明なら `match_greedy()` を参照として併記してください。**

`match_optimal()` は件数が多いと重くなります（`warn_size` / `max_size` で制御、
`block = ` で部分問題に分割）。

---

## 信頼度 `CONFIDENCE`

`CONFIDENCE` は**攻撃者から見える**確信度です。正解を知らないと計算できない量は
入っていないので、「攻撃者が自信のあるものだけ主張したら」という評価に使えます。

既定は `"margin"`（eccentricity）で、
**(2 位のスコア − 1 位のスコア) / 候補スコアの標準偏差**です。
`"tie"`（= 1 / 同点候補数）も選べます。

```r
head(reid_confidence(combined), 3)
#>   ANON_ROW_NUMBER N_CANDIDATES BEST_SCORE SECOND_SCORE TIE_SIZE MARGIN
#> 1               1          200          1            2        1      1
#> 2               2          200          2            2        6      0
#> 3               3          200          2            2        5      0
#>    SD_SCORE ECCENTRICITY CONFIDENCE
#> 1  8.636978    0.1157812  0.1157812
#> 2 10.739584    0.0000000  0.0000000
#> 3 16.687021    0.0000000  0.0000000
```

`TIE_SIZE` / `MARGIN` / `SD_SCORE` / `ECCENTRICITY` はどちらの設定でも同じ値が入り、
`CONFIDENCE` だけが `method` で切り替わります。

> **既定は 2026-07-31 に `"tie"` から `"margin"` へ変わりました**（Issue #44）。
> `"tie"` は連続スコアではほぼ全件が 1.0 に潰れ、精度–再現率曲線の閾値が
> 1 点しか取れませんでした。**割当結果と成功率は変わりません**が、
> `CONFIDENCE` の値と PR 表は過去の出力と比較できません。
> 経緯と実測は [`docs/default-changes.md`](docs/default-changes.md) にあります。

> **eccentricity には尺度の可搬性がありません。** その記録自身の候補スコアの
> 散らばりに対する比なので、数値の範囲はスコア表の性質で決まります
> （実測で、密な数値データは最大 0.45、疎なトランザクションデータは 4.86）。
> **閾値をデータセット間で使い回さないでください。** 必ず実測分布から取ります。

```r
stats::quantile(reid_confidence(combined)$CONFIDENCE, c(0.5, 0.9, 1))
#>        50%        90%       100% 
#> 0.06204356 0.11889747 0.23643010 
```

---

## 評価指標: `reid_evaluate()`

`reid_evaluate(scores, seeds = , top_k = )` は、スコア表 1 本から次をまとめて返します。

| 指標 | 何が分かるか |
|---|---|
| **success rate** | 全体成功率。解析的な厳密値と、シード違いのシミュレーション（平均・標準偏差・範囲）の両方 |
| **baseline** | ランダム割当・最頻値割当での成功率。**これを超えない攻撃は無意味**。`lift` は倍率 |
| **top-k hit rate** | 候補 k 件まで絞れた割合。完全特定でなくても漏洩は起きる |
| **max per-record risk** | 最も危険な 1 レコードのリスク。平均では見えない |
| **precision–recall** | 攻撃者が `CONFIDENCE` に閾値を置き、自信のあるものだけ主張した場合 |

解析値とシミュレーション値は独立に計算されており、**両者が一致することが
「壊れていたら気づける」ためのチェック**になっています。

同点の決着は乱数に依存するため、1 回の実行値だけを信じないでください。
ばらつきだけを見たいときは `reid_stability()` が使えます。

```r
reid_stability(reid_by_num, pairs, "AGE", seeds = 1:20)
#> reid stability over 20 tie-break seeds (trial = 200)
#>   success rate: mean 0.1108  sd 0.0221  range [0.0800, 0.1550]
```

---

## 攻撃者知識モデル (W / M / S)

「成功率 30%」は、**攻撃者が何を知っている前提か**を書かないと解釈できません。
`join_raw_anon_data()` は RAW を総当たりで突き合わせるため、暗黙に
「攻撃者は元データを丸ごと持っている」= 最強の想定を置いています。
上界としては正しいものの、それだけでは現実のリスクを過大評価します。

| レベル | 攻撃者が持つ補助情報 | 対応する現実 |
|---|---|---|
| **W（弱）** | 準識別子の一部のみ | 公開統計・名簿からの推測 |
| **M（中）** | 準識別子一式 + 粗い行動特徴 | 別サービスの会員データを持つ事業者 |
| **S（強）** | RAW レコードそのもの | 元データ保有者、内部犯行、データ流出後 |

```r
k <- attacker_knowledge(
  "M",
  quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char")
)
k
#> attacker knowledge: level M (medium)
#>   visible columns (3): AGE[num], ZIP[char], SEX[char]
#>   withheld (0): 

reid_evaluate(score_by_knowledge(pairs, k), seeds = 1:20)
#> reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
#>   success rate   : 0.9200 exact | simulated mean 0.9203 sd 0.0068 range [0.9100, 0.9300] over 20 seeds
#>   baseline       : random 0.0050 | mode 0.0050   (lift vs random: 184.00x)
#>   top-k hit rate : k=1 0.9200  k=5 1.0000  k=10 1.0000
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 1.0279 : attack 1/200 (0.5%)  precision 1.0000  recall 0.0050
#>     conf >= 0.9199 : attack 2/200 (1.0%)  precision 1.0000  recall 0.0100
#>     conf >= 0.9023 : attack 3/200 (1.5%)  precision 1.0000  recall 0.0150
#>     conf >= 0.8970 : attack 4/200 (2.0%)  precision 1.0000  recall 0.0200
#>     conf >= 0.8813 : attack 5/200 (2.5%)  precision 1.0000  recall 0.0250
#>     ... 187 more threshold(s)
```

AGE + ZIP + SEX の 3 属性が見えるだけで **92%** が特定されます。

3 レベルを横並びで比較するには `reid_knowledge_curve()` を使います。

```r
reid_knowledge_curve(
  pairs,
  quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
  seeds = 1:10
)
#>   level n_visible success_analytic success_mean  success_sd baseline_random
#> 1     W         1        0.1127183       0.1085 0.022242352           0.005
#> 2     M         3        0.9200000       0.9215 0.007472171           0.005
#> 3     S         3        0.9200000       0.9215 0.007472171           0.005
#>        lift max_risk
#> 1  22.54365        1
#> 2 184.00000        1
#> 3 184.00000        1
```

この例では `behavior` / `identifiers` を渡していないため M と S が同じ列集合になり、
数値も一致します。**単一の「再識別率」という数字は、想定する攻撃者を明記して
はじめて解釈できる**、というのがこの表の読み方です。

既定の列定義を手早く使うには `dummy_qi_knowledge()` があります。

---

## ユニシティ

ここまでの指標はすべて「特定の攻撃手法の成功率」なので、良い結果は
「その手法では破れなかった」という弱い主張しか支えません。
**ユニシティは手法に依存しません。** p 個の属性が分かったとき、
他の誰とも区別がついてしまうレコードがどれだけあるかを直接数えます。

```r
unicity_fraction(raw, c("AGE", "ZIP"))
#> [1] 0.94

unicity(raw, attributes = c("AGE", "ZIP", "SEX"), p = 1:3, seed = 1)
#>   p n_subsets exhaustive unicity_mean unicity_sd unicity_min unicity_max
#> 1 1         3       TRUE   0.01833333 0.02753785        0.00        0.05
#> 2 2         3       TRUE   0.42666667 0.44859038        0.11        0.94
#> 3 3         1       TRUE   0.98000000 0.00000000        0.98        0.98
```

200 人のデータで、AGE と ZIP の 2 属性だけで 94% が一意に定まります。

> ユニシティは**確実に**特定できる割合なので、実際の攻撃の期待成功率の
> **下界**です（上界ではありません）。m 人が同じ値を共有するレコードはここでは
> 0 と数えますが、攻撃者は m 択を当てずっぽうで 1/m の確率で当てられます。

### 時空間ユニシティ

位置情報のような (場所, 時刻) の点列では、**わずか数点で個人が一意に定まります**
（de Montjoye et al., "Unique in the Crowd", *Scientific Reports* 3:1376, 2013 では
4 点で 95%）。`spatiotemporal_unicity()` はこれを手元のデータで測ります。

```r
st <- create_dummy_transaction_data(people = 60, size = 20,
                                    spatiotemporal = TRUE, seed = 1)
spatiotemporal_unicity(st, k = c(1, 2, 4), time_resolution = c(1, 24), seed = 1)
#>   k time_resolution space_resolution n_individuals n_evaluated n_points
#> 1 1               1                1            60          60     1170
#> 2 2               1                1            60          60     1170
#> 3 4               1                1            60          60     1170
#> 4 1              24                1            60          60      814
#> 5 2              24                1            60          60      814
#> 6 4              24                1            60          60      814
#>   exhaustive   unicity expected_id_rate mean_anonymity_set
#> 1      FALSE 0.9717321        0.9858661           1.028268
#> 2      FALSE 1.0000000        1.0000000           1.000000
#> 3      FALSE 1.0000000        1.0000000           1.000000
#> 4      FALSE 0.4829888        0.7095300           1.743582
#> 5      FALSE 0.9565045        0.9778047           1.046181
#> 6      FALSE 1.0000000        1.0000000           1.000000
```

時刻を 1 時間刻みから 24 時間刻みに粗くすると、k = 1 のユニシティは大きく下がります。
粗視化そのものは `coarsen_place()` / `coarsen_time()` で個別に使えます。

```r
coarsen_place(c("P001", "P002", "P003", "P004"), resolution = 2)
#> [1] 1 1 2 2
coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)
#> [1] 0 0 0 1 2 2
```

---

## トランザクションデータからマスタ形式へ

1 人が複数行を持つ明細形式のデータは、`transform_transaction_to_master()` で
1 人 1 行のマスタ形式に集約してから使います。

```r
tx <- create_dummy_transaction_data(people = 60, size = 8, seed = 3)

m <- transform_transaction_to_master(
  tx,
  ID = "ID", ROW_NUMBER = "ROW_NUMBER",
  STATIC_NUM   = "NUM_STATIC",   # 人ごとに不変の数値
  DYNAMIC_NUM  = "NUM_DYNAMIC",  # 明細ごとに変わる数値
  DYNAMIC_CHAR = "CHAR"          # 明細ごとに変わるカテゴリ
)

names(m)
#>  [1] "ID"                 "NUM_STATIC"         "NUM_DYNAMIC_MAX"   
#>  [4] "NUM_DYNAMIC_MEAN"   "NUM_DYNAMIC_MEDIAN" "NUM_DYNAMIC_MIN"   
#>  [7] "NUM_DYNAMIC_DIST"   "CHAR_DIST"          "ROWCOUNT"          
#> [10] "ROW_NUMBER"        
```

`DYNAMIC_*` の列は `<列名>_MAX` / `_MEAN` / `_MEDIAN` / `_MIN` の要約と、
値を区切り文字で連結した分布列 `<列名>_DIST`（例 `"0.163:0.175"`）になります。
`_DIST` 列は `score_dist()` / `score_jaccard()` / `score_profile()` の入力に
そのまま使えます。

### 行動そのものを手がかりにする

準識別子を消しても、**行動の形**が残っていれば手がかりになります。

| 関数 | 手がかり |
|---|---|
| `score_count()` | 明細の件数（`ROWCOUNT`） |
| `score_span()` | 分布列の値の幅（最大 − 最小） |
| `score_profile()` | 分布列の構成比の形（件数の多寡を無視できる） |

```r
m_pairs <- join_raw_anon_data(m, m)

reid_evaluate(score_count(m_pairs), seeds = 1:10)
#> reid evaluation: 60 ANON x 60 RAW record(s), 3600 candidate pair(s)
#>   success rate   : 0.2167 exact | simulated mean 0.2400 sd 0.0425 range [0.1833, 0.3167] over 10 seeds
#>   baseline       : random 0.0167 | mode 0.0167   (lift vs random: 13.00x)
#>   top-k hit rate : k=1 0.2167  k=5 0.7500  k=10 0.9333
#>   max per-record risk: 1.0000
#>   precision-recall (threshold on attacker-visible CONFIDENCE, margin):
#>     conf >= 0.8419 : attack 1/60 (1.7%)  precision 1.0000  recall 0.0167
#>     conf >= 0.6990 : attack 2/60 (3.3%)  precision 1.0000  recall 0.0333
#>     conf >= 0.2019 : attack 3/60 (5.0%)  precision 1.0000  recall 0.0500
#>     conf >= 0.0000 : attack 60/60 (100.0%)  precision 0.2167  recall 0.2167
```

**件数を数えただけで、ランダム割当の 13 倍です。** 準識別子を 1 つも使っていません。

区切り文字は `collapse` で変えられます。読み戻す側の `split` にも**同じ文字**を
渡してください。両側ともリテラル文字列として扱われるので、`"|"` や `"."` のような
正規表現のメタ文字も安全に使えます。

```r
m2 <- transform_transaction_to_master(tx, DYNAMIC_NUM = "NUM_DYNAMIC", collapse = "|")
reid_by_dist(join_raw_anon_data(m2, m2), "NUM_DYNAMIC_DIST", split = "|")
```

---

## 関数一覧

エクスポートされている 55 関数のすべてです。

### データ準備

| 関数 | 説明 |
|---|---|
| `join_raw_anon_data(raw, anon)` | RAW × ANON の全候補ペアを作る。列に `RAW_` / `ANON_` の接頭辞が付く |
| `transform_transaction_to_master(dat, ...)` | 明細形式 → 1 人 1 行のマスタ形式 |
| `create_dummy_qi_data(people, seed)` | 準識別子つきダミーデータ |
| `create_dummy_master_data(people)` | ダミーのマスタデータ |
| `create_dummy_transaction_data(people, size, spatiotemporal)` | ダミーの明細データ（位置・時刻つきも可） |

### スコア層

| 関数 | 説明 |
|---|---|
| `score_num(dat, target)` | 数値の絶対差 |
| `score_char(dat, target)` | 文字列の編集距離 |
| `score_num_rank(dat, target)` | 数値の順位差 |
| `score_dist(dat, target)` | 分布列どうしの分布間距離 |
| `score_multi(dat, targets, weights, method, screen)` | 複数列を宣言して統合（`"weighted"` / `"mahalanobis"`） |
| `score_mahalanobis(dat, targets)` | 相関を打ち消したマハラノビス距離 |
| `score_idf(dat, target)` | 値の希少さ（IDF）で重み付けした一致 |
| `score_idf_match(dat, targets)` | 複数列の IDF 重み付き一致の合計 |
| `value_frequencies(dat, target)` | 値ごとの出現数と割合（IDF の中身の確認用） |
| `score_jaccard(dat, target, method)` | 集合の重なり（Jaccard / Dice / Overlap / Tversky） |
| `score_minhash(dat, target, n_hash)` | min-hash による Jaccard の近似 |
| `score_count(dat, target)` | 明細件数の近さ |
| `score_span(dat, target)` | 分布列の値の幅の近さ |
| `score_profile(dat, target, bins)` | 分布列の構成比の形の近さ |
| `score_containment(dat, targets, hierarchy)` | 一般化区画に RAW が含まれるかで絞る |
| `score_scoreboard(dat, targets, tolerance)` | 疎行列向け Scoreboard-RH スコア |

### 候補生成・ブロッキング

いずれも**削減率と再現率（recall）を実測して `blocking` 属性に記録**し、
recall が 1 を下回れば警告します。

| 関数 | 説明 |
|---|---|
| `block_candidates(raw, anon, keys, transform)` | 準識別子の完全一致（または粗視化）で候補を絞る決定的ブロッキング。`keys` にリストを渡すと和集合 |
| `lsh_candidates(raw, anon, target, bands)` | 集合属性向け。min-hash LSH で候補ペアを絞る |
| `top_k_candidates(scores, k, ties)` | スコア表を ANON ごとに上位 k 件へ刈り込む |
| `blocking_recall(candidates, raw, anon)` | 自作の候補集合の削減率と再現率を測る |

### 統合層・軸の診断

| 関数 | 説明 |
|---|---|
| `normalize_scores(scores, method)` | `"range"` / `"zscore"` / `"rank"` / `"none"` で尺度を揃える |
| `combine_scores(scores, weights)` | 複数スコアの加重和 |
| `axis_informativeness(scores, alpha)` | 各軸がランダム割当より有意に良いかの検定 |
| `axis_report(scores)` | 上記を統合スコアから読み戻して表示 |

### 割当層・信頼度

| 関数 | 説明 |
|---|---|
| `match_greedy(scores, seed, confidence, min_confidence)` | 貪欲割当（同点はシードで無作為に決着） |
| `match_optimal(scores, seed, block, dummy_cost)` | 大域最適な 1 対 1 割当（`clue` が必要） |
| `match_scoreboard_rh(scores, phi, assignment)` | 確信度 φ 未満を棄却する割当 |
| `reid_confidence(scores, method)` | `"margin"`（既定）/ `"tie"` の内訳を表で返す |

### 評価

| 関数 | 説明 |
|---|---|
| `reid_evaluate(scores, seeds, top_k)` | ベースライン・top-k・精度再現率を含む総合評価 |
| `reid_stability(reid_fn, ...)` | シード違いのばらつき |
| `reid_result(...)` | 旧来の「成功数 / 試行数」表示 |
| `unicity_fraction(dat, columns)` | 指定した列で一意になるレコードの割合 |
| `unicity(dat, attributes, p, seed)` | p 属性の部分集合を走査したユニシティ曲線 |
| `spatiotemporal_unicity(dat, k, time_resolution, space_resolution)` | (場所, 時刻) k 点でのユニシティ |
| `coarsen_place(x, resolution)` / `coarsen_time(x, resolution)` | 位置・時刻の粗視化 |

### 攻撃者知識

| 関数 | 説明 |
|---|---|
| `attacker_knowledge(level, ...)` | 攻撃者が見てよい列の定義 |
| `dummy_qi_knowledge(level)` | ダミーデータ向けの既定定義 |
| `score_by_knowledge(pairs, knowledge)` | 知識モデルに沿ったスコア |
| `reid_knowledge_curve(pairs, ...)` | W / M / S の横並び比較 |

### 一般化

| 関数 | 説明 |
|---|---|
| `generalization_hierarchy(x)` | `attribute` / `value` / `parent` の表から階層を作る |
| `read_generalization_hierarchy(path, format)` | CSV / YAML から階層を読む |
| `generalization_units()` | 「37歳」のような単位付き表記の単位辞書 |
| `generalize_value(values, attribute, hierarchy, levels)` | 値を階層に沿って粗くする |
| `is_generalized_value(x)` | 値が区間・マスク等の一般化表記かを判定 |
| `containment_counts(dat, targets, hierarchy)` | 公開区画ごとに含まれうる RAW の件数（k 匿名性の実測） |
| `score_containment(dat, targets, hierarchy)` | 含意関係に基づくスコア |

### 従来 API

`reid_by_num()` / `reid_by_char()` / `reid_by_dist()` / `reid_by_num_rank()`

---

## 限界と注意

**このパッケージは研究・検討用です。以下を理解したうえで使ってください。**

- **法令適合や安全性を保証するものではありません。** 個人情報保護法をはじめとする
  法令上の「匿名加工情報」「仮名加工情報」の基準を満たすかどうかの判断には
  使えません。出力される数値は、あくまで**ここに実装された手法で試した結果**です。

- **「再識別されなかった」は「安全である」を意味しません。** 評価ツールの数値は
  安全に見える方向に外れやすく、低い数値ほど疑われにくいという構造的な危険が
  あります。本当に安全な場合と、測定が壊れている場合・攻撃が弱すぎる場合の
  区別はつきません。必ず `reid_evaluate()` のベースラインと比べ、
  ばらつき（`reid_stability()`）と `max per-record risk` を併せて見てください。

- **測り方を間違えると、危険なデータが安全に見えます。** 同じデータでも、
  集合属性を分布距離で測ると 0.0700、Jaccard で測ると 0.9447 になります
  （上の[集合属性](#集合属性買い物かご訪問先)の例）。一般化列に文字列距離を
  誤用すると実リスクの約 1/4 しか出ません（Issue #40: 0.1017 対 0.4450）。
  **低い数値が出たときこそ、手法が対象に合っているかを疑ってください。**

- **ブロッキングは、再現率が 100% でなければリスクを過小報告します。**
  `block_candidates()` / `lsh_candidates()` / `top_k_candidates()` は候補ペアを
  捨てるので、正解ペアを落とせばそのレコードは**永久に特定できなくなり、
  成功率は下がります**。上の実測では、キーを `ZIP`（recall 1.0000）から
  `AGE`（recall 0.3900）に変えただけで 0.92 が 0.38 になりました。
  必ず `attr(candidates, "blocking")` の recall を確認し、
  1 でないなら結果を**下界**として報告してください。`reid_evaluate()` は
  総当たりでない候補表を自動検出して出力の先頭に明示しますが、
  候補ゼロになった ANON レコードはそもそも母数に入りません。
  総当たりが可能な規模なら、絞らずに測るのが正解です（Issue #36）。

- **`match_optimal()` は重なりが部分的なときリスクを過小報告します。**
  重なりが不明なら `match_greedy()` の値を併記してください（Issue #15）。

- **`CONFIDENCE`（eccentricity）の閾値をデータセット間で使い回さないでください。**
  尺度がスコア表ごとに違います。実測分布の分位点から取ってください（Issue #16, #44）。

- **カテゴリ的な一般化は自動検出できません。** 「千代田区 → 東京都」のような
  置換は構造から見えないので、`score_char()` は停止せず低い数値を返します。
  一般化階層の宣言は利用者の責任です（Issue #40）。

- **未実装の手法で破られる可能性は、このツールでは測れません。** 実装済みなのは
  数値・文字列・順位・分布・集合・一般化・疎行列のスコア、IDF 重み、多属性統合、
  貪欲割当と大域最適割当、ユニシティ（属性・時空間）です。
  機械学習ベースのリンケージ、軌跡データそのものへの攻撃、
  差分プライバシの観点からの評価などは未実装です。実装候補の全体像は
  [`docs/reid-method-candidates.md`](docs/reid-method-candidates.md) にあります。

- **攻撃者知識の想定を必ず明記してください。** 既定の `join_raw_anon_data()` は
  最強の攻撃者（S）を仮定します。想定を書かない成功率の数字は解釈できません。

- **同点の決着は乱数に依存します。** 単一のシードの結果を結論にしないでください。

- **既定値は変わることがあります。** 過去の報告書と数値を比べる前に
  [`docs/default-changes.md`](docs/default-changes.md) を確認してください。

---

## ドキュメント

| ドキュメント | 内容 |
|---|---|
| [`docs/reid-method-candidates.md`](docs/reid-method-candidates.md) | 再識別手法カタログ（24 件）と評価フレームの設計。実装済み・未実装の全体像 |
| [`docs/default-changes.md`](docs/default-changes.md) | 既定値の変更履歴。同じデータで過去と違う数値が出る変更の記録 |
| [`docs/implementation-plan.md`](docs/implementation-plan.md) | Issue 候補と依存関係、実装の進め方 |
| [`docs/lessons-learned.md`](docs/lessons-learned.md) | 調査・修正・統合作業から得た知見 |
| [`docs/verify-readme-examples.R`](docs/verify-readme-examples.R) | このページのコード例を通し実行し、`#>` の出力と関数の網羅を機械照合する（[実行ログ](docs/verify-readme-examples-log.txt)） |
| [`docs/investigation/`](docs/investigation/) | 調査ログと実測値、実装比較ベンチマーク（各手法の検証スクリプトと実行ログ） |
| `reidentify.pdf` | roxygen 生成のリファレンスマニュアル |

関数ごとの詳細は R から参照できます。

```r
?reid_evaluate
help(package = "reidentify")
```

公開ページ: <https://gghatano.github.io/reidentify/>

---

## ライセンス

MIT License. [`LICENSE`](LICENSE) を参照してください。
