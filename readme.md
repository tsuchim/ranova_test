
# R で 反復測定分散分析 を行うテスト
- 複数の教育法の効果を検証するために、同じ被験者に対して複数のテストを反復して行った。
- このデータを用いて、反復測定分散分析を行うスクリプトを書いてみる
- 反復回数は2回 (事前テスト と 事後テスト1回のみ)
- テスト1・2・3の3つのテストは、それぞれ異なる能力を測定するテスト

## 勉強履歴
2024/5/15
- 反復測定において同じ被験者が同じテストを受けているので、対応有りの分散分析を行う必要がある
- 3つのテストは、それぞれ異なるテストなので、多変量分散分析を行う必要がある
- 今は教育法の違いによる影響の違いは検定しないので、物事をシンプルにするために個別に検定するようにした
- エクセルが出力するCSVを読み込むように、文字コードをShift-JISに変更した