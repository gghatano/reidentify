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

### 方法 1: GitHub から直接（base R のみ、1 行）

```r
install.packages(c("dplyr", "magrittr", "stringi", "tibble", "philentropy"))

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

以下はコピー＆ペーストでそのまま動きます（出力は実行結果そのままです）。

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
```

実際の出力:

```
reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
  success rate   : 0.1127 exact | simulated mean 0.1108 sd 0.0221 range [0.0800, 0.1550] over 20 seeds
  baseline       : random 0.0050 | mode 0.0050   (lift vs random: 22.54x)
  top-k hit rate : k=1 0.1127  k=5 0.3728  k=10 0.5363
  max per-record risk: 1.0000
  precision-recall (threshold on attacker-visible CONFIDENCE):
    conf >= 1.0000 : attack 21/200 (10.5%)  precision 0.0952  recall 0.0100
    conf >= 0.5000 : attack 50/200 (25.0%)  precision 0.1400  recall 0.0350
    conf >= 0.3333 : attack 94/200 (47.0%)  precision 0.1277  recall 0.0600
    conf >= 0.2500 : attack 148/200 (74.0%)  precision 0.1250  recall 0.0925
    conf >= 0.2000 : attack 169/200 (84.5%)  precision 0.1213  recall 0.1025
    ... 4 more threshold(s)
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

```
[1] スコア層   score_num() / score_char() / score_dist() / score_num_rank()
                  → (RAW_ROW_NUMBER, ANON_ROW_NUMBER, SCORE) のロング形式

[2] 統合層     combine_scores(list(...), weights = ...)
                  → 複数属性のスコアを 1 本にまとめる

[3] 割当層     match_greedy(scores, seed = )
                  → (ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT)
```

| 層 | 関数 | 役割 |
|---|---|---|
| スコア層 | `score_num()` | 数値属性の距離 |
| | `score_char()` | 文字列属性の一致度 |
| | `score_dist()` | 分布列（`"1:2:3"` 形式）の分布間距離 |
| | `score_num_rank()` | 数値の順位の近さ |
| 統合層 | `combine_scores()` | 複数スコアを正規化して加重和 |
| 割当層 | `match_greedy()` | ANON 1 件ごとに最良の RAW を選ぶ（同点はシードで無作為に決着） |

複数属性を組み合わせる例:

```r
s_age <- score_num(pairs, "AGE")
s_zip <- score_char(pairs, "ZIP")

combined <- combine_scores(list(s_age, s_zip))
picked   <- match_greedy(combined, seed = 1)

head(picked, 5)
#>   ANON_ROW_NUMBER RAW_ROW_NUMBER CONFIDENCE RESULT
#> 1               1            121  1.0000000  FALSE
#> 2               2              2  0.1666667   TRUE
#> 3               3            166  0.2000000  FALSE
#> 4               4              4  1.0000000   TRUE
#> 5               5              5  1.0000000   TRUE

reid_evaluate(combined, seeds = 1:20)
```

```
reid evaluation: 200 ANON x 200 RAW record(s), 40000 candidate pair(s)
  success rate   : 0.4303 exact | simulated mean 0.4363 sd 0.0121 range [0.4100, 0.4600] over 20 seeds
  baseline       : random 0.0050 | mode 0.0050   (lift vs random: 86.06x)
  top-k hit rate : k=1 0.4303  k=5 0.8387  k=10 0.9940
  max per-record risk: 1.0000
```

AGE 単独の 11.3% が、ZIP を足しただけで 43.0% に上がります。

1 行目に注目してください。`CONFIDENCE` が 1.0（＝同点候補が 1 件しかない、
攻撃者から見れば最も確信できるケース）にもかかわらず `RESULT` は `FALSE` です。
**確信度の高さは正しさを意味しません。**

> `CONFIDENCE` は**攻撃者から見える**確信度（同点候補が 1 件なら 1.0、6 件なら 1/6）
> です。正解を知らないと計算できない量は入っていないので、
> 「攻撃者が自信のあるものだけ主張したら」という評価に使えます。

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
#>         lift max_risk
#> 1   22.54365        1
#> 2  184.00000        1
#> 3  184.00000        1
```

この例では `behavior` / `identifiers` を渡していないため M と S が同じ列集合になり、
数値も一致します。**単一の「再識別率」という数字は、想定する攻撃者を明記して
はじめて解釈できる**、というのがこの表の読み方です。

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

---

## トランザクションデータからマスタ形式へ

1 人が複数行を持つ明細形式のデータは、`transform_transaction_to_master()` で
1 人 1 行のマスタ形式に集約してから使います。

```r
tx <- create_dummy_transaction_data(people = 50)

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
`_DIST` 列は `score_dist()` / `reid_by_dist()` の入力にそのまま使えます。

区切り文字は `collapse` で変えられます。読み戻す側の `split` にも**同じ文字**を
渡してください。両側ともリテラル文字列として扱われるので、`"|"` や `"."` のような
正規表現のメタ文字も安全に使えます。

```r
m <- transform_transaction_to_master(tx, DYNAMIC_NUM = "NUM_DYNAMIC", collapse = "|")
reid_by_dist(join_raw_anon_data(m, m), "NUM_DYNAMIC_DIST", split = "|")
```

---

## 関数一覧

### データ準備

| 関数 | 説明 |
|---|---|
| `join_raw_anon_data(raw, anon)` | RAW × ANON の全候補ペアを作る。列に `RAW_` / `ANON_` の接頭辞が付く |
| `transform_transaction_to_master(dat, ...)` | 明細形式 → 1 人 1 行のマスタ形式 |
| `create_dummy_qi_data(people, seed)` | 準識別子つきダミーデータ |
| `create_dummy_master_data(people)` / `create_dummy_transaction_data(people, size)` | ダミーのマスタ / 明細データ |

### スコア層・統合層・割当層

| 関数 | 説明 |
|---|---|
| `score_num()` / `score_char()` / `score_dist()` / `score_num_rank()` | 属性ごとのスコア表 |
| `combine_scores(scores, weights)` | 複数スコアの統合 |
| `match_greedy(scores, seed)` | 貪欲割当 |

### 評価

| 関数 | 説明 |
|---|---|
| `reid_evaluate(scores, seeds, top_k)` | ベースライン・top-k・精度再現率を含む総合評価 |
| `reid_stability(reid_fn, ...)` | シード違いのばらつき |
| `unicity_fraction(dat, columns)` / `unicity(dat, attributes, p)` | ユニシティ |
| `reid_result(...)` | 旧来の「成功数 / 試行数」表示 |

### 攻撃者知識

| 関数 | 説明 |
|---|---|
| `attacker_knowledge(level, ...)` / `dummy_qi_knowledge(level)` | 攻撃者が見てよい列の定義 |
| `score_by_knowledge(pairs, knowledge)` | 知識モデルに沿ったスコア |
| `reid_knowledge_curve(pairs, ...)` | W / M / S の横並び比較 |

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

- **実装済みなのは基礎的な手法に限られます。** 数値・文字列・順位・分布の
  距離による単純なマッチングと、貪欲割当だけです。多属性の統合距離、
  大域最適割当（ハンガリアン法）、機械学習ベースの手法、時系列・軌跡データへの
  攻撃などは未実装です。実装候補の全体像は
  [`docs/reid-method-candidates.md`](docs/reid-method-candidates.md) にあります。
  **未実装の手法で破られる可能性は、このツールでは測れません。**

- **攻撃者知識の想定を必ず明記してください。** 既定の `join_raw_anon_data()` は
  最強の攻撃者（S）を仮定します。想定を書かない成功率の数字は解釈できません。

- **同点の決着は乱数に依存します。** 単一のシードの結果を結論にしないでください。

---

## ドキュメント

| ドキュメント | 内容 |
|---|---|
| [`docs/reid-method-candidates.md`](docs/reid-method-candidates.md) | 再識別手法カタログ（22 件）と評価フレームの設計。実装済み・未実装の全体像 |
| [`docs/implementation-plan.md`](docs/implementation-plan.md) | Issue 候補と依存関係、実装の進め方 |
| [`docs/lessons-learned.md`](docs/lessons-learned.md) | 調査・修正・統合作業から得た知見 |
| [`docs/investigation/`](docs/investigation/) | 調査ログと実測値、実装比較ベンチマーク |
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
