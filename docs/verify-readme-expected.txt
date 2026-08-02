# docs/verify-readme-expected.txt
#
# docs/verify-readme-examples.R が README.md を実行して得るはずの件数と、
# 実行中に出るはずの警告・メッセージ。Issue #62 の対策。
#
# なぜ固定するか: ハーネスの危険な壊れ方は「赤くなる」ことではなく
# 「黙って検査対象が減る」ことである。実際 #62 では、コメント 1 行が
# ブロック 1 つを検査対象から外し、README の改竄が exit 0 になった。
# 唯一の兆候は誰も見ていない 34 → 33 という数字だった。
#
# 意図して README を変えたときは、変更が意図どおりであることを確認して
#   Rscript docs/verify-readme-examples.R . --rewrite
# で再生成する。手で編集してもよいが、減らす方向の編集は理由を書くこと。

blocks_run: 37
blocks_skipped: 4
output_units: 47

# 実行中に signal される警告・メッセージ。順序込みで完全一致を要求する。
# ここに無い警告が出ても、ここにある警告が出なくても落ちる。
condition: README.md:377 +1 | WARNING: block_candidates(): blocking discarded 122 of 200 true pair(s) (recall 0.39). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").
condition: README.md:417 +1 | WARNING: block_candidates(): blocking discarded 24 of 200 true pair(s) (recall 0.88). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").
condition: README.md:432 +1 | WARNING: lsh_candidates(): blocking discarded 23 of 200 true pair(s) (recall 0.885). Any reidentification rate measured on this candidate set is a LOWER bound. See attr(x, "blocking").
condition: README.md:719 +2 | WARNING: combine_scores(): the components are on very different scales -- scores[[1]] has 26.1x the weighted spread of scores[[2]] (sd 13.8 vs 0.531). The widest component decides the ranking and the others only break its ties. That is harmless when the dominant component is also the most informative, but when it is not, adding attributes LOWERS the measured reidentification rate and the result understates the risk. Put the components on a common scale first -- combine_scores(normalize_scores(scores, "range")) or score_multi() -- or set weights to compensate. Pass scale_check = "none" if the scale gap is intended.
