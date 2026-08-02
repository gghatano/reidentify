# reidentify (開発版)

## 一般化列のガードを全スコアに広げた（#100）

Issue #40 のガードは `score_char()` / `score_dist()` / `score_num_rank()` の
**3 関数に手で付けられていました**。宣言できるスコア型は 8 つあり、
`attacker_knowledge()` は 8 つすべてを受け付けます。実測（AGE を
`[30,40)` に一般化、N=200）:

| | 変更前 | 変更後 |
|---|---|---|
| `score_num` / `char` / `dist` / `rank` / `count` / `span` | 停止 | 停止 |
| `score_idf` / `score_profile` | **完走** | 停止 |
| `score_jaccard` / `score_minhash` / `score_scoreboard` / `score_mahalanobis` | **完走** | 停止 |

`score_containment(AGE, SEX)` が 0.0500 を報告する同じデータで、
黙って完走する経路は 0.0050 —— ランダム割当のベースラインちょうど —— を
返していました。**10 倍の過小報告で、エラーも警告もありません。**

ガードは関数ではなく**列の解決**（`reid_score_columns()`）に付け直しました。
逃げ道は各スコアの `generalized = "warn"` / `"ignore"` のみです。

## `"containment"` を宣言できるようにした（#100）

`reid_score_types()` に `"containment"` を追加しました。これまで
`attacker_knowledge(c(AGE = "containment"))` はエラーで、**一般化データを扱う
W/M/S 利用者には「止まる 6 型」か「黙って過小報告する 2 型」しか
選択肢がありませんでした。**

```r
attacker_knowledge("M", quasi_identifiers = c(AGE = "containment",
                                              AREA = "containment"))
```

`score_multi()` は `"idf"` と同様、`"containment"` 列を**まとめて**
`score_containment()` に渡します（領域は足すのではなく**積を取る**ため）。
`score_multi()` / `score_by_knowledge()` / `reid_knowledge_curve()` に
`hierarchy` / `rules` を追加し、カテゴリ一般化も宣言経路から扱えます。

## 再発防止

`reid_generalized_guard_policy()` に、エクスポートされた全 `score_*()` 関数の
一般化列に対する方針（`"refuse"` / `"containment"` / `"delegates"`）を
宣言しました。`test-generalized-column-guard.R` が
これを **NAMESPACE の実際のエクスポートと突き合わせ**、さらに全 `"refuse"`
エントリを一般化フィクスチャに対して実行します。**スコア関数やスコア型を
追加してガードを忘れると、テストが落ちます。**

## 非互換

`score_idf()` / `score_profile()` / `score_jaccard()` / `score_minhash()` /
`score_scoreboard()` / `score_mahalanobis()` / `score_idf_match()` は、
一般化列に対して**エラーになります**。従来の数値が必要なら
`generalized = "ignore"` を明示してください。

## 測定が止まっているのに「安全」と表示されていた（#101, #109）

いずれも `docs/lessons-learned.md` §2 の失敗形である。**同じデータに対して
過去より大きいリスクが報告される。**実測値は
`docs/investigation/empty-candidate-set-benchmark-log.txt` にある。

### 候補ゼロを警告するようになった（#101）

`score_containment()` は、**候補が 1 件も残らなかった ANON レコード**が
あると警告するようになった。候補ゼロは「全候補が同点 1」を意味し、
`match_greedy()` は一様に引くので、そのレコードは**ランダムベースラインを
そのまま報告値に足す**。全件がそうなると `lift = 1.00x`、つまり
「攻撃は無意味」と読める出力になる。

既存の 3 つの検査（`blocked` / `n_true_missing` / `truth_coverage`）は
**どれもこれを見つけられない。**候補表は全 `n_anon × n_raw` 行を保ち、
真のペアも入ったままだからである。件数自体は #20 から
`attr(scores, "candidate_count")` に正確に記録されていたが、誰も読んでいなかった。
`reid_evaluate()` がこれを読み、`n_zero_candidate` として返し、印字するようにした。

### RAW 側も ANON 側と同じパーサで読むようになった（#101）

`"37歳"` のように**単位が RAW 側にだけ書かれている**と、`as.numeric()` が
`NA` を返して候補が全滅していた。

| RAW の書き方 | success | lift | 候補ゼロ |
|---|---|---|---|
| `37`（修正前後とも） | 0.1200 | 24.00x | 0/200 |
| `37歳`（修正前） | 0.0050 | **1.00x** | **200/200** |
| `37歳`（修正後） | 0.1200 | 24.00x | 0/200 |

RAW 側は**点区間だけ**を受け付ける（`"30-39"` は領域に広げない）。
1 文字の ASCII 英字（`m` / `g` / `y`）は RAW 側では単位として扱わない
（`"1m"` を 1 と読むと、乱数英数字列の 0.78% が数値になる。実測は上記ログ）。

### 抑止表記が「値」ではなく「抑止」として読まれるようになった（#101）

`-` `--` `?` `.` `N/A` `null` `unknown` `missing` `不明` `未回答` `無回答`
`非公開` `秘匿` などをワイルドカード（= 全 RAW 値に一致）として読む。
これまで `*` だけが抑止として扱われ、ほかは文字列完全一致に落ちて候補ゼロになっていた。

**`999` や `-1` のような数値センチネル、および文字列 `"NA"` は意図的に
含めない。**ワイルドカードは候補集合を*広げる*ので、誤検出はリスクを
*低く*報告する方向に効く。これらは代わりに候補ゼロ警告が名指しする。

### 階層の attribute 名が列名と一致しないとエラーになった（#109）

`score_containment(hierarchy = )` に渡した階層の `attribute` が `targets` の
どれとも一致しない場合、これまで**黙って無視**していた。`"zip"`（小文字）や
`"POSTCODE"` を渡した出力は `hierarchy = NULL` と 1 ビットも違わず、
郵便番号のフィクスチャでは **106 倍の過小報告**だった。
生成側の `generalize_value()` は以前から同じ不一致で停止しており、
その非対称が失敗を見えなくしていた。

一部の `targets` だけを覆う階層は従来どおり有効である。

### 既定の `rules` がマスク列を読むようになった（#109）

`"135****"` のような**末尾 `*` のマスク**を、`rules = NULL`（= 全列 `"auto"`）
でも prefix として読む。従来はカテゴリとして扱われて誰にも一致せず、
`rules = c(ZIP = "prefix")` を明示した場合と比べて **106 倍の過小報告**だった。
`generalization_evidence()` は内部で以前から同じ値に `"prefix"` を選んでいた。

`rules = c(ZIP = "exact")` は従来どおり完全一致である。

---

# reidentify 3.0.0 (2026-08-01)

**従来 API（`reid_by_*()` / `reid_result()`）を削除しました。**
これが 3.0.0 のすべてです。ほかの機能・既定値・返り値は 2.0.0 から
変わっていません。

## 破壊的変更 — 関数を削除した

以下の 5 関数を削除しました（#84）。

| 削除した関数 | 置き換え |
|---|---|
| `reid_by_num()` | `match_greedy(score_num(pairs, "COL"))` |
| `reid_by_char()` | `match_greedy(score_char(pairs, "COL"))` |
| `reid_by_dist()` | `match_greedy(score_dist(pairs, "COL"))` |
| `reid_by_num_rank()` | `match_greedy(score_num_rank(pairs, "COL"))` |
| `reid_result()` | `reid_evaluate(scores)` |

2.0.0 の時点でこれらはすでに 3 層 API の薄いラッパで、内部では同じ
`score_*()` + `match_greedy()` を呼んでいました。同じ引数・同じシードなら
上の置き換えは**数値としても同一の結果**を返します（2.0.0 の
`test-layers.R` がシードごとに突き合わせていました）。

`reid_result()` だけは戻り値の形が変わります。`" method: X , success /
trial :  23 / 200"` という文字列を返していましたが、この 2 つの数は
`reid_evaluate()` の `per_seed$success` / `per_seed$trial` から読めます。
`reid_evaluate()` はそれに加えてランダム割当のベースライン・シード違いの
ばらつき・レコードごとのリスクを併記します。**成功率を単独で出さない**のは
意図的で、比較対象のない成功率は読めないためです
（`docs/lessons-learned.md` §2）。

### 移行例

```r
## 2.0.0
r <- reid_by_num(pairs, "AGE", seed = 1)
print(reid_result(r, method = "AGE"))

## 3.0.0
m <- match_greedy(score_num(pairs, "AGE"), seed = 1)   # 割当そのもの
reid_evaluate(score_num(pairs, "AGE"))                 # 評価つきの報告
```

`reid_stability()` は残っています。引数 `reid_fn` には
`(dat_raw_anon, target, ..., seed)` を取る**攻撃**を渡します。従来 API を
渡していた箇所は、スコア層と割当層を 1 行で束ねてください。

```r
## 2.0.0
reid_stability(reid_by_num, pairs, "AGE", seeds = 1:20)

## 3.0.0
attack_num <- function(dat, target, seed) {
  match_greedy(score_num(dat, target), seed = seed)
}
reid_stability(attack_num, pairs, "AGE", seeds = 1:20)
```

### 削除にあたって確認したこと

従来 API のテストの多くは、旧 API の入口を使って**新 API と共有する内部**を
検証していました。テストごと消せば検証が黙って弱くなり、CI は緑のまま
誰も気づきません（`docs/lessons-learned.md` §2 の失敗様式）。そこで、削除
した各テストが守っていた振る舞いを新 API の入口で書き直しています。

* 引数 `row_number` の配線（`reid_prefixed_columns()`）→ 4 つの `score_*()`
* 同点処理と NA ガード（`resolve_min_distance_ties()`）→ `match_greedy()`
* デコイ列（`raw_target` など）による列取り違え → 4 つの `score_*()`
* タイブレークのシード固定・入力行順非依存・不偏性 → `match_greedy()`
* 返り値の契約（1 ANON 1 行、`success <= trial`）→ `match_greedy()` /
  `reid_evaluate()`
* ノイズ耐性・単調性 → `match_greedy(score_*())`

`reid_result()` が持っていた「`ANON_ROW_NUMBER` の重複を拒む」防御は、
より上流の `validate_unique_candidate_pairs()`（スコア表の入口）が
引き継いでいます（#60）。

## 内部の整理

* `R/reidentify.R` を削除し、新 API が依存していた内部ヘルパーを移しました。
  `with_local_seed()` → `R/utils.R`、`resolve_min_distance_ties()` →
  `R/match.R`、`check_raw_anon_columns_exist()` → `R/score.R`、
  `validate_split()` / `parse_dist_values()` / `calc_KL()` /
  `distribution_distance()` → `R/distance.R`、`reid_stability()` →
  `R/evaluate.R`。**公開 API の振る舞いは変わりません。**
* 旧 API 専用だった内部ヘルパー `as_reid_output()` を削除しました。
* エクスポート関数は 55 個から 50 個になりました。

# reidentify 2.0.0 (2026-08-01)

1.0.0.0（2019 年）以来の最初の更新です。API を 3 層（スコア層 / 統合層 /
割当層）に分け、評価指標を増やし、いくつかの既定値を変えました。

**同じコードに同じデータを与えても、以前と違う数値が出る箇所があります。**
これは安全性評価ツールでは特に混乱の元なので、下の「破壊的変更」を先に
読んでください。既定変更の詳細な理由と実測値は `docs/default-changes.md`
(<https://github.com/gghatano/reidentify/blob/master/docs/default-changes.md>)
にあります。

バージョンを 1.0.0.0 から 2.0.0 に上げたのは、(a) 4 成分のバージョンが R の
慣行から外れていたことと、(b) 以下のとおり利用者の得る数値が変わる変更が
入っており semver では major に当たることの 2 点によります。

## 破壊的変更 — 既定値を変えた

* `match_greedy()` / `match_optimal()` / `reid_evaluate()` / `reid_per_anon()` の
  `confidence` の既定を `"tie"` から `"margin"` に変えました（#44）。
  `CONFIDENCE` 列の値がすべて変わり、`reid_evaluate()` の
  `precision_recall` の行数が増えます（連続スコアでは `"tie"` はほぼ全件が
  1 に潰れ、閾値が 1 点しか取れませんでした）。**リスクそのもの**
  （割当結果・`RESULT`・`success_analytic`・`success_mean`・`baseline`・
  `lift`・`top_k`・`RISK`・`max_risk`）**は変わりません。**
  以前の数値は `confidence = "tie"` で再現できます。

* 同点判定に相対許容誤差 `tolerance` を導入し、既定を
  `sqrt(.Machine$double.eps)`（約 1.5e-8）にしました（#61）。
  `reid_evaluate()` / `reid_confidence()` / `match_greedy()` /
  `match_optimal()` が受け付けます。丸め誤差が同点判定に効いていたデータでは
  `success_analytic` / `max_risk` / `RISK` / `TIE_SIZE` / `MARGIN` /
  `ECCENTRICITY` / `CONFIDENCE` / `precision_recall` / `top_k` が変わります。
  実測では、同じデータを整数単位で書くか 1/10 単位で書くかだけで
  `max_risk` が 0.5 と 1.0 に分かれていました（n=200 の構成で、真の
  per-record risk はどのレコードも厳密に 1/2）。整数スコアや、候補スコアが
  8 桁以上離れているデータでは何も変わりません。
  以前の数値は `tolerance = 0` で再現できます。

* `unicity_fraction()` / `unicity()` / 時空間ユニシティのキー生成を
  作り直しました（#58, #70, #73）。列ごとに同値クラスの整数コードへ直して
  から連結し、double 列は `tolerance` を通します。以前は `as.character()` で
  連結していたため、double が 15 桁で印字されて `0.1 + 0.2` と `0.3` が
  同一視される一方、`"\r"` を含む文字列は別レコードを衝突させ、
  **属性を足すほど一意率が下がる**ことがありました。時空間ユニシティでは
  衝突がリスクを低く見せる向きに働いていました。一意率の数値が変わります。

## 破壊的変更 — 誤りを直したことで数値が変わる

いずれも「壊れていたものを直した」変更ですが、過去の報告書と数値が
合わなくなるため破壊的変更として挙げます。

* `reid_by_dist()` にタイブレーク処理が無く、割り算の分母が候補行数に
  なっていました（#25）。同点が多いほど分母が膨らみ、**報告される成功率が
  小さく出ていました。**
* `score_char()` が一般化された列で停止せず、実リスクの約 1/4 という
  もっともらしい数値を返していました（#40）。
* `distribution_distance()` の長さ合わせで、RAW と ANON の件数差が距離に
  混入していました（#5）。分位点ベクトルの比較に変えました。
* `calc_KL()` が KL ダイバージェンスの定義（非負性）を満たしていませんでした
  （#4）。総和正規化を入れました。
* タイブレークが行順に依存していました（#3）。乱数化し、`seed` 引数と
  ばらつきの報告（`reid_stability()`）を足しました。
  `reid_by_num_rank()` の `rank(ties.method = "random")` という第 2 の乱数源も
  seed に含めました。
* `row_number` 引数が受け取られるだけで使われておらず、行番号列を改名した
  データではすべての `reid_by_*()` が失敗していました（#28）。
* `split` 引数が正規表現として解釈され、`"|"` や `"."` を指定すると
  壊れていました（#32）。リテラル文字列として扱います。
* `transform_transaction_to_master()` の集計列名が、渡した引数の個数で
  変わっていました（#26）。列数によらず同じ名前になります。
* 重複した候補ペアがあると、解析値とシミュレーション値が**揃って**誤り、
  自己整合チェックが機能していませんでした（#60）。
  `reid_evaluate()` / `match_greedy()` が重複ペアを拒否します。

## 新しい警告 — 返り値は変わらない

いずれも「測定が静かに壊れている状態」を見つけるためのもので、
SCORE 列や返り値そのものは 1 ビットも変わりません。意図的な使い方であれば
黙らせられます。

* `score_multi()` / `score_by_knowledge()` が、情報を持たない軸を検出して
  警告します（既定 `screen = "warn"`、#35 / #43）。無情報な軸を等重みで
  統合すると単一属性より成功率が下がるため、黙って過小報告になります。
  `screen = "none"` で従来どおり。
* `combine_scores()` が、重みをかけた後の成分の標準偏差の比が 10 倍を
  超えると警告します（既定 `scale_check = "warn"`、#57）。
  `scale_check = "none"` で従来どおり。
* `score_mahalanobis()` が、共分散（ridge 適用後）の条件数が 100 を超えると
  警告します（#59）。あわせて、統合結果が最良の単一軸を下回ったときにも
  警告します。既定の `ridge` と `cov_from` は変えていません。
* `lsh_candidates()` の `attr(x, "blocking")` が `"reid_blocking"` クラスに
  なり、`method` / `n_raw` / `n_anon` / `reduction` / `n_true_pairs` /
  `n_true_pairs_kept` / `recall` が増えました（#36）。既存フィールドは
  すべて残ります。`recall < 1` のとき警告します。
* `reid_evaluate()` が `n_true_missing`（正解が候補集合に無い件数）を
  印字するようになりました（#56）。以前は計算しながら表示していなかったため、
  「測定できていない」状態が「安全」と読めていました。

## 新機能

* スコア層 / 統合層 / 割当層への API 分離（#11）。
  `score_*()` → `combine_scores()` → `match_greedy()` / `match_optimal()`。
* 評価指標の拡充（#12）— ランダム / 最頻値のベースラインと `lift`、
  精度–再現率曲線、Top-k、レコード別リスク、`max_risk`。
* 攻撃者知識モデル W / M / S（#13）— `score_by_knowledge()`。
* 多属性統合距離（#14）— 正規化・重み付き和・Mahalanobis。
* グローバル最適割当（#15）— `clue::solve_LSAP()` によるハンガリアン法。
* 信頼度付きマッチング（#16）— マージン / eccentricity と閾値による絞り込み。
* IDF 重み付き一致スコア（#17）— 希少値の一致を重く数える。
* 集合類似度によるマッチング（#18）— Jaccard / Tversky / min-hash / LSH。
* 一般化・区間整合マッチングと一般化階層の外部定義（#20）。
* ユニシティ測定（#21）— p 属性で一意になる割合。
* 活動プロファイルによるスコア（#22）— 件数・曜日分布・活動期間。
* Scoreboard-RH 型スコアリング（#23）— 疎データ向け。
* 時空間ユニシティ（#24）— k 点で特定できる割合。
* ブロッキングによる候補削減（#36）— 総当たり結合の n^2 を避けつつ、
  取りこぼした正解ペアの割合（`recall`）を必ず報告する。

## ドキュメントと開発基盤

* vignette「なぜそう測るのか」を追加しました（#75）。
* README をパッケージに追加し（#10）、実装との整合を CI で機械照合するように
  しました（#47 / #50 / #62）。README のコード例の `#>` 出力は毎回実行して
  突き合わせています。
* 公開ページ <https://gghatano.github.io/reidentify/> を追加し（#48）、
  そこに載る「テスト件数」「エクスポート関数の個数」を CI で照合するように
  しました（#63）。
* `testthat` によるテスト基盤を導入しました（#7）。
* roxygen コメントを関数本体の外に出し、ドキュメントを再生成できるように
  しました（#8）。
* `R CMD check --as-cran` を CI で走らせ、NOTE でも落とすようにしました（#67）。
* 2019 年にビルドされた同梱 PDF（55 関数中 46 個が載っていなかった）を
  削除しました（#66）。
* DESCRIPTION を現在の実装に合わせ、`Authors@R` / `URL` / `BugReports` /
  `Depends: R (>= 4.1.0)` を整備しました（#77）。

# reidentify 1.0.0.0

* 初版（GitHub のみ、CRAN 未公開）。`reid_by_char()` / `reid_by_num()` /
  `reid_by_num_rank()` / `reid_by_dist()` による 1 発呼び出しの API。
