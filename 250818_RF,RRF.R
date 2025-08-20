#pure解析を自動でやりたい

#ライブラリ読み込み
library(dplyr); library(tidyr); library(stringr); library(zoo); library(ggplot2)

install.packages("zoo")


getwd()

# 1) CSV読み込み
df <- read.csv("250818_RF,RRF.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")

# 1列目の列名をCyclesに（何が来ても強制で）
names(df)[1] <- "Cycles"

# 2) tidy化（ラベル→ condition / rep）
#   ──A案：repは「_1/_2/...」で表記している場合（推奨：ハイフンと衝突しない）
long <- df |>
  pivot_longer(-Cycles, names_to = "label", values_to = "fluor") |>
  mutate(
    fluor     = as.numeric(fluor),
    condition = str_remove(label, "_\\d+$"),                 # 末尾の "_数字" を削除
    rep       = as.integer(str_extract(label, "(?<=_)\\d+$")),
    rep       = tidyr::replace_na(rep, 1L)                   # repが無い列は1扱い
  )

## ──B案：repを「-1/-2/...」で表記しているならこちらに差し替え（※条件名内のハイフンと紛れやすい）
# long <- df |>
#   pivot_longer(-Cycles, names_to = "label", values_to = "fluor")_


# 3) 5サイクル「中心化」移動回帰の傾き（SLOPE）を算出
slope_fun <- function(y) {
  x <- seq_along(y)
  if (sum(!is.na(y)) < 2) return(NA_real_)  # データ点が1以下ならNA
  unname(coef(lm(y ~ x))[2])                # 傾きだけ返す
}

long_slope <- long |>
  arrange(condition, rep, Cycles) |>
  group_by(condition, rep) |>
  mutate(
    slope5 = zoo::rollapply(fluor, width = 5, FUN = slope_fun,
                            align = "center", fill = NA)    # ±2点＋当該点の5点で傾き
  ) |>
  ungroup()


# 4) 各（condition×rep）で slope5 の最大値を取得
per_rep <- long_slope |>
  group_by(condition, rep) |>
  summarise(max_slope5 = max(slope5, na.rm = TRUE), .groups = "drop")


# 5) 条件ごとの 平均・SD・n（rep間で集計）

# 5.1) 並べたい順番にソート
order <- c("∆RF-1/2/3/, ∆RRF, ∆EF-G",
           "∆RF-1/2/3/, ∆RRF",
           "∆RF-1/2/3/",
           "∆RF-3, ∆EF-G",
           "∆RF-3, ∆RRF",
           "∆RF-3",
           "∆RF-1/3",
           "∆RF-2/3",
           "RF-1/2 >> prfA",
           "EF-G >> fusA",
           "RRF >> frr",
           "RF-1/2 >> prfA, RRF >> frr",
           "EF-G >> fusA, RF-1/2 >> prfA",
           "RRF >> frr, EF-G >>fusA",
           "RF-1/2 >> prfA, RRF >> frr, EF-G >>fusA")

# condition を factor に変換
per_cond <- per_cond |>
  mutate(condition = factor(condition, levels = order))


unique(per_cond$condition)
setdiff(unique(per_cond$condition), order)  # 実データにあるのに order に無い
setdiff(order, unique(per_cond$condition))  # order にあるけど実データに無い








per_cond <- per_rep |>
  group_by(condition) |>
  summarise(mean_slope = mean(max_slope5, na.rm = TRUE),
            sd_slope   = sd(max_slope5,   na.rm = TRUE),
            n          = dplyr::n(),
            .groups    = "drop") |>
  arrange(condition)

# 6) バープロット（平均±SD）— 縦軸は「傾き（蛍光値/サイクル）」
ggplot(per_cond, aes(condition, mean_slope)) +
  geom_col(fill = "white", color = "black", width = 0.5) +   # ←白塗り＋黒枠 
  geom_errorbar(aes(ymin = mean_slope - sd_slope, ymax = mean_slope + sd_slope), width = 0.2) +
  scale_y_continuous(limits = c(0, 1000),
                     expand = expansion(mult = c(0, 0.05)))+
  labs(x = NULL, y = "Max 5-cycle slope (fluor/cycle)") +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x  = element_text(size = 10, angle = 55, hjust = 1), # 文字大きく＆斜め
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(t = 20, r = 20, b = 40, l = 30) # 上:t, 右:r, 下:b, 左:lに余白（単位=pt）
  )




