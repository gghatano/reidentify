# 敵対的検証と修正の記録

- 実施日: 2026-07-29 〜 2026-07-30
- 対象リビジョン（出発点）: `ce86e9c` (`master`)
- 成果ブランチ: `fix/adversarial-p4p5`（コミット `627114c` → `7f8306e` → `fd69a76`）
- 実行環境: R 4.6.1 / dplyr 1.2.1 / magrittr 2.0.5 / tibble 3.3.1 / philentropy 0.10.0 / stringi 1.8.7 / testthat 3.3.2 / roxygen2 8.0.0 / pkgload 1.5.3（devtools・Rtools・LaTeX はいずれも未導入）
- 検証スクリプト: `docs/investigation/adversarial-probe-*.R`（4 本）

## 経緯

`ce86e9c` の時点でテストは 1 件も無く、`README` も無かった。まず「動かすには何が要るか」を調べ、`R/tmp/dev.R` のシナリオを再現するスモークテストを書いて 11 ステップ全 PASS を得た。しかしこれはハッピーパス 1 本にすぎず、根拠として弱い。そこで結論そのものを壊しにいく敵対的検証を行い、見つかった欠陥を 6 フェーズに分けて修正した。

各フェーズの実装はサブエージェントに任せ、**フェーズごとに、報告とは独立に上記スクリプトで測り直してから次に進めた**。サブエージェントの報告を受け入れずに済んだ例が実際に 2 件ある（後述の「想定が外れた点」）。

> **注意（作業ツリーの競合について）**
> フェーズ 3 の途中で、同じ作業ツリー `workspace/reidentify` を別セッションが並行編集していることが判明した（`8b206c6` は当該セッションのコミット）。未コミットの修正が一度失われかけたため、フェーズ 4 以降は `git worktree` で `workspace/reidentify-p4p5` に分離し、フェーズ 1〜3 の成果を `627114c` としてコミットして固定した上で作業を続けた。

## 検証方法

基準データは「ANON が RAW の完全コピー」＝**真の再識別率が 100%** になる 30 件のマスタデータ。ここで 30/30 を返さない手法は壊れている、という判定基準を置いた。

```
Rscript docs/investigation/adversarial-probe-no-attach.R    # 依存の attach 前提
Rscript docs/investigation/adversarial-probe-properties.R   # 正しさの性質・縮退・計算量
Rscript docs/investigation/adversarial-probe-impact.R       # 誤りが報告値にどう出るか
Rscript docs/investigation/adversarial-probe-distance.R     # 距離関数の公理
```

検証した性質:

| 観点 | 内容 |
|---|---|
| 恒等性 | ANON = RAW の完全コピーなら success == trial == N |
| 無情報性 | 対象列を乱数で置換したら成功数がベースライン（≒1）付近に落ちる |
| 自立性 | `library(reidentify)` だけで（dplyr を attach せずに）全関数が動く |
| 列名衝突 | 内部変数と同名の列がデータに実在しても結果が変わらない |
| 同点処理 | 結果行数が ANON レコード数と一致する（分母が水増しされない） |
| 型の誤用 | 文字列列を数値前提の関数に渡したときに黙って通らない |
| 決定性 | 同じ入力を複数回実行して結果が一致する |
| 距離の公理 | 対称性 `d(x,y)==d(y,x)`、自己距離 `d(x,x)==0`、KL の非負性 |
| 計算量 | `join_raw_anon_data` の行数・メモリ・所要時間の N 依存 |

## 判定サマリ

初回のスモークテストは全 PASS だったが、敵対的検証では **12 件の欠陥**が出た。うち 6 件は「実際より安全」と誤報告する方向、つまり安全性検証ツールとしては最悪の壊れ方だった。

| # | 欠陥 | 誤りの方向 | 修正前 | 修正後 | 該当フェーズ |
|---|---|---|---|---|---|
| 1 | `pull` / `n()` / `%<>%` の import 漏れ | 実行不能 | attach 無しで 8 関数中 **5 つが実行時エラー** | 8/8 動作 | P1 |
| 2 | 列名衝突でサイレント誤答 | **過小評価** | **1 / 30**（無警告） | 30 / 30 | P2 |
| 3 | `pull(all_of())` も同じ罠 | **過小評価** | 誤った列を返す | base の `[[ ]]` に統一 | P2 |
| 4 | `reid_by_dist` の同点処理欠落 | **過小評価** | 30 / **108**（分母水増し） | 30 / 30 | P3 |
| 5 | 非数値列で無言の 0/0 | **過小評価** | 0 / 0 | 変換不能な要素を示すエラー | P3 |
| 6 | `row_number` 引数が未実装 | 実行不能 | 渡すと落ちる | 30 / 30 | P4 |
| 7 | `ties.method="random"` で非決定的 | 再現不能 | 実行毎に変動 | `"min"` で固定 | P4 |
| 8 | `tibble::data_frame()` deprecation | 警告 | 警告あり | 解消 | P4 |
| 9 | NA に実在の順位を割り当て | **過小評価** | 距離 0 の「確信を持った誤答」 | エラー | P5 |
| 10 | 存在しない列名のエラーが不可解 | 診断性 | base R の意味不明なエラー | 列名を名指しするエラー | P5 |
| 11 | `calc_KL` が負値（KL 非負性違反） | 定義違反 | **-0.664** | 0.768 | P6 |
| 12 | `distribution_distance` の件数依存 | 尺度不整合 | 相関 0.63 / 件数差で 3.86 倍 | 相関 0.335 / 2.63 倍 | P6 |

### 欠陥 2（最重要）の詳細

各 reid 関数は `target` から列名文字列を組み立て、tidyselect に**裸のシンボル**として渡していた。

```r
raw_target <- paste("RAW_", target, sep = "")
dplyr::select(RAW = `raw_target`, ...)
```

tidyselect は「裸のシンボルをまず実在の列名として探し、無ければ環境の変数値を列名として使う」フォールバックで動く。したがって `raw_target` という名前の列がデータに実在すると、**エラーも警告も出さずにそちらを掴む**。真値 30/30 のデータで 1/30 を返した。

このフォールバック自体は tidyselect 1.1.0 で deprecated であり（テスト実行時に 17 件の警告が出ていた）、将来削除されれば全面的に壊れる。`all_of()` と base の `[[ ]]` に置き換えて解消した。

### 欠陥 4 の詳細

`reid_by_num` / `reid_by_char` / `reid_by_num_rank` には最小距離が同点の場合に 1 件へ絞る処理があるが、`reid_by_dist` にだけ欠落していた。同点の RAW 行が全部残るため、ANON 30 件に対し結果が 108 行になり、`reid_result()` の `trial`（= `nrow`）が水増しされて **100% を 27.8% と報告**していた。

4 関数で重複していた同点処理を内部ヘルパー `resolve_min_distance_ties()` に集約して解消。再発防止として `reid_result()` は `ANON_ROW_NUMBER` に重複があればエラーで停止するようにした。

## 想定が外れた点

記録として残す。

1. **初回の見立て「現行 dplyr/tibble では動かない箇所が多そう」は外れた。** `select(RAW = \`raw_target\`)` のような古い書き方は、上記の環境フォールバックのおかげで今も「たまたま動く」。ただしこれは動作の保証ではなく、欠陥 2 の原因そのものだった。
2. **`pull(dplyr::all_of(var))` は `select()` と違って安全ではない。** P2 でこれが判明し、`pull()` 系はすべて base の `[[ ]]` に置き換えた。
3. **別調査で報告された「`distribution_distance` の相関 0.99」は再現しなかった**（合成データでの実測は 0.63）。相関係数の絶対値は測定設定に依存する。本質は「距離が要素数に応じてスケールし、レコード数の異なる人同士を同一尺度で比較できないこと」であり、この機構自体は実在した。
4. **`reid_by_dist(BIN_DIST)` は 30/30 にはならない**（15/30）。離散列で値そのものが真に衝突しているためで（30 人中 17 パターン）、同点処理を直しても解消しない。恒等性の検証にはエントロピーの高い列を使う必要がある。

## 品質指標

| | `ce86e9c` | `fd69a76` |
|---|---|---|
| テスト | **存在せず** | **272 assertions / FAIL 0 / WARN 0** |
| `R CMD check --no-manual` | 1 WARNING + 2 NOTE | **Status: OK**（ERROR/WARNING/NOTE すべて 0） |
| tidyselect deprecation 警告 | 17 件 | 0 件 |

テストは 7 ファイル: `test-self-contained.R`（別プロセスを起動して attach 無しで検証）、`test-column-selection.R`、`test-tie-and-na.R`、`test-args-and-determinism.R`、`test-statistical-properties.R`（無情報性は 10 シード平均、単調性は 8 回平均で判定）、`test-boundary-cases.R`、`test-contract.R`、`test-distance-metrics.R`。

`R CMD check` は LaTeX 未導入のため `--no-manual` 付き。tarball を作って独立に実行しても Status: OK。

## 残った課題

| 課題 | 内容 |
|---|---|
| `join_raw_anon_data` の N² 総当たり | 実測 N=800 → 62 万行 / 195MB / 0.9 秒。外挿すると N=5,000 で約 2,500 万行・7〜8GB。実用上の上限は数千件で、ドキュメント記載も無い |
| `reid_by_dist` の中〜高ノイズ域での成功数低下 | 件数依存の除去に伴い sd=0.1 で 18.1 → 16.6 / 30（50 シード平均）。旧実装が分布の形ではなくレコード件数を偶発的なシグナルとして使っていた分であり、除去は意図した結果。件数を使いたい場合は `ROWCOUNT` 列を明示的に使う経路がある |
| `calc_KL` が未使用 | どの公開関数からも呼ばれていない（`reid_by_dist` 内でコメントアウト）。`philentropy` 依存はこの関数のためだけに残っている |
| `reid_by_num_rank` とレコード抑制 | RAW と ANON で行数が異なる場合、ランクが異なる母集団基準で計算される。手法上の特性だが解釈に注意が必要 |
| README 無し | 未作成 |
| 内部関数の roxygen | 変更履歴の説明（「以前はこうだった」「フェーズ 6 で」）が長く残っている。履歴はコミットメッセージ側にあるので、ドキュメントは仕様の記述だけに絞る余地がある |

## 再現方法

```
# パッケージのインストール（ユーザーライブラリへ）
R.exe CMD INSTALL --no-multiarch --library=%LOCALAPPDATA%\R\win-library\4.6 .

# テスト
Rscript -e "testthat::test_local('.')"

# チェック（LaTeX 未導入のため --no-manual 必須）
R.exe CMD build .
R.exe CMD check --no-manual --no-build-vignettes reidentify_1.0.0.0.tar.gz

# 敵対的検証スクリプト（テスト化済みなので通常は不要）
Rscript docs/investigation/adversarial-probe-no-attach.R
Rscript docs/investigation/adversarial-probe-properties.R
Rscript docs/investigation/adversarial-probe-impact.R
Rscript docs/investigation/adversarial-probe-distance.R
```

PowerShell では `R` が組み込みエイリアス（`Invoke-History`）に食われるため、`R CMD` を使うときは `R.exe` と拡張子付きで書く必要がある。`Rscript` は素で動く。

`adversarial-probe-properties.R` と `adversarial-probe-impact.R` の「期待」欄は修正前の状態を前提に書かれている。修正後に実行すると、欠陥 5 と 11 の箇所は意図通りエラーで停止する。
