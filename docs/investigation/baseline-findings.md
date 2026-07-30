# 現行実装の動作確認とバグ再現記録 (Issue #1)

- 実施日: 2026-07-29
- 対象リビジョン: `ce86e9c` (`fix/adversarial-findings` = `master`) の **コミット済み内容**
- 実行環境: R 4.6.1 / dplyr 1.2.1 / magrittr 2.0.5 / tibble 3.3.1 / philentropy 0.10.0 / stringi 1.8.7 / openssl 2.4.2 / testthat 3.3.2 / roxygen2 8.0.0 / pkgload 1.5.3（devtools は未導入）
- 再現スクリプト: `docs/investigation/baseline-probe.R`
- 実行ログ全文: `docs/investigation/baseline-log.txt`

## 実行方法

`devtools` が無いため `pkgload::load_all()` でソースを読み込んでいる。

```
Rscript docs/investigation/baseline-probe.R
```

> **注意（測定の独立性について）**
> 調査中、作業ツリーの `R/reidentify.R` が本調査とは無関係に書き換えられていたため、
> 掲載した数値はすべて **`git show HEAD:R/*.R` で取り出したコミット済みソースを
> 別ディレクトリに展開して再実行した結果** である。作業ツリーの状態には依存しない。

---

## 判定サマリ

| # | 指摘 (`docs/reid-method-candidates.md` §2.1) | 判定 | 補足 |
|---|---|---|---|
| #2 | 列名変数の tidyeval 非対応 | **別事象（要 Issue 修正）** | エラーにならない。非推奨警告付きで「たまたま動く」。ただし列名衝突時に静かに誤る |
| #3 | タイブレークの行順依存 | **再現（ただしバイアスの向きは非再現）** | 行順依存は確定。成功率の系統的な上振れは観測されず |
| #4 | `calc_KL` の正規化 | **再現** | 負値が出る。KL の非負性を満たさない |
| #5 | `distribution_distance` の長さ合わせ | **再現** | 件数差と距離の相関 0.99 |
| #6 | NAMESPACE の import 漏れ | **再現** | `pull` / `%<>%` / `n()` は attach 無しで実行時エラー |

さらに、**指摘リストに無かった重大な不具合を 1 件検出**（O-5: `reid_by_dist` の試行数水増し）。

---

## #2 列名変数の tidyeval 非対応 — 判定: 別事象

Issue 本文は「変数 `raw_target` の中身が参照されない」と書いているが、**これは誤り**。
tidyselect は列名として解決できない裸のシンボルを呼び出し元環境の変数にフォールバックするため、
`select(RAW = \`raw_target\`)` は意図どおり `RAW_NUM` 列を選ぶ。ただし非推奨警告が出る。

```
!! WARNING: Using an external vector in selections was deprecated in tidyselect 1.1.0.
ℹ Please use `all_of()` or `any_of()` instead.
```

`reid_by_num` / `reid_by_num_rank` / `reid_by_char` / `reid_by_dist` /
`transform_transaction_to_master` はいずれも**現時点では正常に完走する**。

### ただし実害は存在する（修正は必要）

1. **列名衝突で静かに誤る。** 入力に `raw_target` という名の列が実在すると、
   変数より列が優先され、警告も出ずに誤った列で距離計算が行われる。

   ```
   ---- 罠列付きデータで reid_by_num ----
     RAW_ROW_NUMBER ANON_ROW_NUMBER  RAW      ANON DISTANCE RESULT
   1              1               1 -999 0.3392494 999.3392   TRUE
   ```

   `RAW` が `-999`（罠列の値）になり、`DISTANCE` が 999 台になっている。
   エラーにならないぶん、tidyeval 非対応より危険。

2. **将来の tidyselect で完全に壊れる。** 現在は deprecation warning だが、
   いずれエラー化する。

3. `transform_transaction_to_master` で `STATIC_CHAR = NULL` を渡すと、
   `select(\`STATIC_CHAR\`)` は警告のみで黙って何も選ばない。

**結論**: 修正内容（`all_of()` / `.data[[var]]` への置換）は妥当なので Issue #2 はそのまま進めてよい。
ただし**背景の記述（「変数の中身が参照されない」）は事実と異なるので訂正が必要**。
真の根拠は「非推奨 API であること」と「列名衝突時の silent failure」。

---

## #3 タイブレークの行順依存 — 判定: 再現（バイアスの向きは非再現）

`people = 50`、`BIN` 列（値が 3 種しかないため同点が多発）で測定。
ANON 1 件あたりの最小距離同点数は中央値 27。

### 行順依存は確定

| 条件 | 成功率 |
|---|---|
| merge 出力そのままの行順 | 0.0600 |
| 行順シャッフル 200 回 | 平均 0.0576 / sd 0.0232 / 範囲 [0.0200, 0.1400] |
| ランダムタイブレーク 200 シード | 平均 0.0582 / sd 0.0275 |

同一データ・同一手法にもかかわらず、**入力の並べ方だけで成功率が 0.02〜0.14 まで動く**
（平均比で ±130% 程度）。再現性・検証可能性の観点で明確な欠陥。

### バイアスの実測値 — 「高く出る方向」は観測されなかった

| 人数 | 元の行順 | RAW 昇順 | ランダム平均 | **バイアス（元順 − 乱）** |
|---|---|---|---|---|
| 50 | 0.0600 | 0.0600 | 0.0582 | **+0.0018** |
| 100 | 0.0300 | 0.0300 | 0.0306 | **−0.0006** |
| 300 | 0.0100 | 0.0100 | 0.0115 | **−0.0015** |

いずれもランダムタイブレークの標準偏差（sd 0.0275）よりはるかに小さく、**符号も一定しない**。
`docs/reid-method-candidates.md` §2.1-4 の
「ダミーデータでは ROW_NUMBER と ID が相関しているため成功率が実力より高く出る方向のバイアスが入りうる」
は、**この設定では再現しない**。

理由は 2 点:

1. `reid_by_*` は突合に `ROW_NUMBER` しか使っておらず、`ID` は距離計算に一切関与しない。
   したがって「ID と ROW_NUMBER の相関」は成功率に効かない（`cor = 1` ではあるが無関係）。
2. 決定的タイブレーク（各同点群の先頭 = 最小 `RAW_ROW_NUMBER`）が当たるのは
   「自分が同点群の最小値である ANON」だけ。BIN が 3 値なので同点群は 3 つ、
   よって成功は常にちょうど 3 件 = 0.06。RAW 昇順でも降順でも 3 件で一致する
   （降順なら各群の最大値が当たるため、やはり 3 件）。
   一方ランダムタイブレークの期待値は `mean(1/同点数) = 0.06`。**両者が理論的に一致する。**

### 修正方針への含意

- ランダムタイブレーク化 + シード引数 + 複数シード平均、という Issue #3 のスコープは
  **依然として正当**。ただし根拠は「成功率の水増しを解消する」ではなく
  **「行順という無関係な入力に結果が左右されるのを止め、分散を明示する」**に置き換えるべき。
- 決定的タイブレークは成功を「同点群の先頭レコード」に集中させるため、
  **レコード別リスク（#12）が構造的に歪む**。全体平均が同じでも誰が危ないかの推定が壊れる。
  こちらの方が実害として大きい。

---

## #4 `calc_KL` が KL ダイバージェンスの定義を満たしていない — 判定: 再現

`x = 1:2:3:4`, `y = 2:2:2:2` の場合:

- 現行の max 正規化: `x/max(x)` の総和 = 2.5、`y/max(y)` の総和 = 4 → **どちらも確率分布でない**
- 正しい sum 正規化での KL(x‖y) = **0.1535607**（log2 基準。自然対数なら 0.1064401）
- `calc_KL(x, y)` の実測値 = **−1.311278**

**KL ダイバージェンスは定義上必ず 0 以上**なので、負値が出た時点で定義を満たしていない。

ランダムな分布ペア 50 組での確認:

```
ランダムな分布ペア 50 組: 負の値が 11 件 /50
値域: [ -1.5949 , 4.6112 ]
```

`philentropy::KL()` は総和 1 でないベクトルを渡しても 0.10.0 では警告を出さない
（`philentropy::KL(rbind(c(.25,.5,.75,1), c(1,1,1,1)))` も同じ −1.311278 を返す）ため、
呼び出し側で気づけない。

### 再識別への実害（順位への影響）

距離関数として重要なのは絶対値より**候補の順位**なので、そこを測った。
1 つの ANON に対する 8 候補で、現行 `calc_KL` と sum 正規化 KL の順位を比較:

```
 cand calc_KL_current KL_sum_norm rank_current rank_true
    6         -0.9780      0.1943            1         3
    7         -0.1617      0.0902            4         1
```

- Spearman 相関 = **0.7857**（完全一致ではない）
- **argmin が一致しない**（現行は候補 6、正しくは候補 7 を選ぶ）

つまり最近傍の選択そのものが変わる。順位が保たれるから実害なし、とは言えない。

### 設計判断

`calc_KL` は現在 **どこからも呼ばれていない**（`reid_by_dist` の該当行は
`R/reidentify.R:135` でコメントアウト済み）。したがって、

- **総和正規化への修正**を行う（ゼロ要素は平滑化が必要になる旨を doc に明記）
- 同時に、分布比較の本命は D17（分位点ベクトル + Wasserstein, Issue #19）である旨を
  doc に書いて誘導する

の両方を行うのが妥当。**非推奨化のみで済ませるのは、名前が `calc_KL` である以上
「KL を計算する関数が KL を返さない」状態を残すことになり不適切。**

---

## #5 `distribution_distance` の長さ合わせ — 判定: 再現

### 件数差が距離に混入する

同一母集団（`runif(500)`）から件数だけ変えて標本抽出。**分布形状は同じ**なので
理想的には距離はほぼ一定であるべき。

| n_x | n_y | 件数差 | 距離 |
|---|---|---|---|
| 10 | 10 | 0 | 0.1687 |
| 10 | 11 | 1 | 0.1033 |
| 10 | 13 | 3 | 0.1993 |
| 10 | 16 | 6 | 0.5690 |
| 10 | 20 | 10 | 0.9518 |
| 10 | 30 | 20 | 1.4617 |
| 10 | 50 | 40 | 3.8722 |

**cor(件数差, 距離) = 0.99**。指摘どおり件数差がそのまま距離に乗っている。

分位点で揃えた別の確認でも `d(a10,a10)=0`, `d(a10,a20)=0.385`, `d(a10,a40)=1.679` と単調増加。

### 併せて判明した追加の欠陥

**長さが等しいと `sort()` が一切走らない。** 現行実装は差分パディングをした側だけ
`sort()` するため、長さが同じ 2 本はソートされないまま要素ごとに引き算される。

```
distribution_distance('3:1:2', '1:2:3') = 6
```

`{1,2,3}` と `{1,2,3}` は同じ多重集合なので距離は **0 であるべき**。
分布間距離としては誤り。実運用では `transform_transaction_to_master` が
`paste(sort(.), collapse=)` で整列済みの `_DIST` 列を作るため顕在化しないが、
`distribution_distance` を単体で使うと壊れる。

参考: 形状が大きく異なり件数が同じペアでは距離 363.4。
件数差由来の寄与（〜3.9）は絶対値としては小さいが、
**形状が似ている候補どうしの比較では支配的になりうる**（＝取り違えを起こす領域で効く）。

`.**2` の magrittr パイプは意図どおり動作（`distribution_distance('1:2','3:4') = 8`）。

---

## #6 NAMESPACE の import 漏れ — 判定: 再現

`NAMESPACE` に **`pull` / `%<>%` / `n` / `median` のいずれも存在しない**ことを
`parseNamespaceFile()` で確認。

dplyr / magrittr を **attach しない**状態（Imports だけの正しい利用形態）で実行すると:

| 関数 | 結果 |
|---|---|
| `reid_result()` | `Error: could not find function "pull"` |
| `reid_by_char()` | `Error: could not find function "pull"` |
| `reid_by_num_rank()` | `Error: could not find function "%<>%"` |
| `transform_transaction_to_master()` | `Error in n(): could not find function "n"` |

`library(dplyr); library(magrittr)` を実行した後は**すべて正常に動く**。
つまり本パッケージは現状「利用者が dplyr と magrittr を attach していること」に
暗黙に依存しており、パッケージとして自己完結していない。

使用箇所（コミット済みソースの行番号）:

- `pull`: `R/reidentify.R:48, 79, 82, 128, 131`（計 5 箇所）
- `%<>%`: `R/reidentify.R:228, 236`（計 2 箇所）
- `n()`: `R/transform_transaction_to_master.R:51`
- `median`: `R/transform_transaction_to_master.R:36`

`median` だけは実行時エラーにならない。`stats` が既定で search path に載っているため。
ただし `R CMD check` では NOTE 対象なので `@importFrom stats median` は必要。

---

## O. 指摘リストに無かった検出事項

### O-5. `reid_by_dist` にタイブレーク処理が無く、試行数が水増しされる — **新規・重大**

`reid_by_num` / `reid_by_char` / `reid_by_num_rank` は
`filter(RAW_ROW_NUMBER == RAW_ROW_NUMBER[1])` で ANON 1 件につき 1 行に絞るが、
**`reid_by_dist` にはこの処理が無い**（`R/reidentify.R:140-146`）。

同点を強制した入力（ANON 20 件、各 ANON に距離 0 の RAW が 10 件）で実測:

```
ANON 件数 = 20  / 出力行数 = 200  / ANON 重複あり = TRUE
reid_result の分母:  method: dist , success / trial :  20 / 200
```

- 出力が **20 行ではなく 200 行**（10 倍）
- `reid_result()` は行数をそのまま試行数にするため **「20 / 200 = 10%」** と報告する
- 正しくは ANON 20 件に対する成功率を報告すべき

**成功率の分母が壊れているため、`reid_by_dist` の評価結果は現状そのままでは信用できない。**
Issue #3（タイブレーク）のスコープに含めるのが自然。

### O-6. `transform_transaction_to_master` の集計列名が引数の個数で変わる

`DYNAMIC_NUM` に渡す列が 1 個か複数かで `summarise_all` の命名規則が変わる。

- 1 列: `MAX, MEAN, MEDIAN, MIN`（元の列名が付かない）
- 2 列: `NUM_DYNAMIC_MAX, NUM_DYNAMIC_2_MAX, NUM_DYNAMIC_MEAN, ...`

`R/tmp/dev.R` は `NUM_DYNAMIC_MEAN` を前提にしているため、1 列指定では動かない。

### O-7. `reid_by_*` の返り値スキーマが不揃い

| 関数 | 返り値の列 |
|---|---|
| `reid_by_num` | `RAW_ROW_NUMBER, ANON_ROW_NUMBER, RAW, ANON, DISTANCE, RESULT` |
| `reid_by_char` | 入力の全列 + `DISTANCE, RESULT`（**入力依存**） |
| `reid_by_num_rank` | `ANON_ROW_NUMBER, RAW_ROW_NUMBER, ANON_<t>, RAW_<t>, ANON_RANK, RAW_RANK, DISTANCE, RESULT` |
| `reid_by_dist` | `RAW_ROW_NUMBER, ANON_ROW_NUMBER, DISTANCE, RESULT` |

列順も列構成も揃っていない。Issue #11（3 層 API 分離）で統一すべき。

### O-8. `R/tmp/dev.R` はそのままでは動かない

`dev.R:56` が `join_row_anon_data()` を呼んでいるが、実際の関数名は `join_raw_anon_data()`。

### O-9. `tibble::data_frame()` の非推奨警告は実際に出る（Issue #9）

```
Warning: `data_frame()` was deprecated in tibble 1.1.0.
ℹ Please use `tibble()` instead.
ℹ The deprecated feature was likely used in the reidentify package.
```

`create_dummy_master_data()` / `create_dummy_transaction_data()` の両方が該当。
返り値のクラスは `tbl_df/tbl/data.frame` で `tibble()` と同一なので、置換の影響は無い。

---

## 後続 Issue へのフィードバック

| Issue | 必要な修正 |
|---|---|
| **#2** | 背景の記述が事実と異なる。「変数の中身が参照されない」→「非推奨 API であり、列名衝突時に静かに誤る」に訂正。スコープ自体は妥当 |
| **#3** | 「成功率が実力より高く出る方向のバイアス」は本測定では再現せず（実測 +0.0018 / −0.0006 / −0.0015）。根拠を「行順依存による再現性欠如」と「レコード別リスクの歪み」に置き換える。**併せて O-5（`reid_by_dist` の試行数水増し）をスコープに追加すべき** |
| **#4** | 「非推奨化」ではなく「総和正規化への修正 + D17 への誘導」を選択。理由は argmin が変わる実害があり、かつ関数名と挙動の乖離を残すべきでないため |
| **#5** | 指摘どおり。加えて「長さが等しいと sort が走らない」対称性の欠陥も同時に直す必要がある |
| **#6** | 指摘どおり。`median` に対する `@importFrom stats median` も忘れずに追加 |
| **#11** | O-7（返り値スキーマの不揃い）を統一対象として明記 |
| **#12** | O-5 により `reid_by_dist` の試行数が壊れている点を前提として織り込む |
