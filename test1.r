# CSVファイルのフォーマット
# 教育法, テスト1.反復1, テスト2.反復1, テスト3.反復1, テスト1.反復2, テスト2.反復2, テスト3.反復2

# データの特性
# 独立変数 は 教育法(と反復)
# 従属変数 は テスト1・テスト2・テスト3 (のスコア) (多変量)
# 反復因子 は 反復(被験者の対応あり)

# 必要なパッケージのインストールと読み込み
library(car)

# CSVファイルの読み込み
raw_data <- read.csv("data1.csv", header = TRUE, fileEncoding = "CP932")

# 対応付けで使うかもしれないので、行番号を被験者IDとして割り振る
raw_data$被験者ID <- seq_len(nrow(raw_data))

# データの整形
data <- reshape(raw_data,
                idvar = "被験者ID",
                varying = list(c("テスト1.反復1", "テスト1.反復2"),
                               c("テスト2.反復1", "テスト2.反復2"),
                               c("テスト3.反復1", "テスト3.反復2")),
                v.names = c("テスト1", "テスト2", "テスト3"),
                timevar = "反復",
                times = c("反復1", "反復2"),
                direction = "long")

# データの整形
data$反復 <- as.factor(data$反復)

# 教育法間の違いを見ないなら、教育法ごとに対応ありのMANOVAを実行
for (教育法 in unique(data$教育法)) {
  # 教育法ごとのデータを抽出
  data_sub <- data[data$教育法 == 教育法, ]

  # 対応ありのMANOVAの実行
  model <- lm(cbind(テスト1, テスト2, テスト3) ~ 反復, data = data_sub)
  result <- Anova(model, type = "II")
  # 結果の表示
  print(paste("教育法:", 教育法))
  summary(model)
  print(result)
}