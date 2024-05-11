# CSVファイルの読み込み
# 教育法, テスト1.反復1, テスト2.反復1, テスト3.反復1, テスト1.反復2, テスト2.反復2, テスト3.反復2
raw_data <- read.csv("data1.csv", header = TRUE)

# データの整形
# 独立変数 は 教育法
# 従属変数 は テスト(のスコア)
# 反復因子 は 反復(回数)
data <- reshape(raw_data,
                varying = list(c("テスト1.反復1", "テスト1.反復2"),
                               c("テスト2.反復1", "テスト2.反復2"),
                               c("テスト3.反復1", "テスト3.反復2")),
                v.names = c("テスト1", "テスト2", "テスト3"),
                timevar = "反復",
                times = c("反復1", "反復2"),
                direction = "long")

# 教育法と反復を因子として扱う
data$教育法 <- as.factor(data$教育法)
data$反復 <- as.factor(data$反復)

# データの表示
print(data)

# 分析モデルの指定
model <- aov(テスト1 + テスト2 + テスト3 ~ 教育法 * 反復, data = data)

# モデルオブジェクトの表示
print(model)

# 分析結果の表示
summary(model)