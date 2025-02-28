#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ホーム > 各種データ・資料 > 地球環境・気候 > 地球温暖化 > 
#   気温・降水量の長期変化傾向 > 日本の月平均気温
#   https://www.data.jma.go.jp/cpdinfo/temp/jan_jpn.html
#
#   気象庁提供データ
#     https://www.data.jma.go.jp/cpdinfo/temp/list/csv/mon_jpn.csv
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


pacman::p_load(
  tidyverse,
  janitor,
  ggthemes,
  slider,
  supportR)


########################################
# 四捨五入関数定義
########################################

round2 = function(x, d = 0) {
  p = 10 ^ d
  return((x * p * 2 + 1) %/% 2 / p)
}


########################################
# データを読み込み
########################################

CSV.url <- "https://www.data.jma.go.jp/cpdinfo/temp/list/csv/mon_jpn.csv"
df.org <- read.csv(CSV.url, skip = 1, header = FALSE) |> 
  pivot_longer(col = !V1,
               names_to = "Month",
               values_to = "anom") |> 
  filter(!is.na(anom)) |> 
  mutate(Month = as.integer(gsub("V", "", Month)) - 1) |> 
  rename(Year = 1)

########################################
# 各地点の各年各月の偏差を算出して平均
# 各月の5年移動平均を追加
########################################

ANOM <- df.org |> 
  group_by(Month) |> 
  mutate(mavg5 = slide_vec(anom, .f = mean, .before = 4)) |> 
  mutate(mavg5 = lead(mavg5, 2)) |> 
  mutate(mavg5 = ifelse(Year < 1900, NA, mavg5))


########################################
# 各月の回帰係数算出
########################################

Trend <- ANOM |> 
  group_by(Month) |> 
  do(trend = lm(anom ~ Year, data = .)) |> 
  mutate(coef = round2(trend$coefficients[2] * 100, 2)) 


########################################
# 月を指定してプロット
########################################

MONTH = 2
ANOM.mon <- ANOM |> filter(Month == MONTH)
BREAKS <- c(1898, seq(1910, 2010, 10), max(ANOM.mon$Year))
TITLE <- paste0(month.name[MONTH], ": Average Temperature Anomaly")

ggplot(ANOM.mon, aes(Year, anom)) +
  geom_hline(yintercept = 0, color = "gray95") +
  geom_line(color = "gray80", linewidth = 0.5) +
  geom_point(color = "gray80", size = 2.2) +
  geom_line(aes(Year, mavg5), color = "lightblue", linewidth = 1.0) +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  guides(x = guide_axis(minor.ticks = FALSE, cap = "both"),
         y = guide_axis(minor.ticks = TRUE, cap = "both")) +
  scale_x_continuous(breaks = BREAKS) +
  annotate("text", x = 1915, y = 3.5, 
           label = paste("Trend ＝", Trend$coef[MONTH], "(°C/100years)"), 
           color = "gray90", size = 4.25) +
  ylim(c(-4, 4)) +
  theme_hc(style = "darkunica") +
  labs(x = "", y = "Anomaly (°Celsius)",
       title = TITLE) + 
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1), color = "gray90"),
        axis.text  = element_text(size = rel(0.95),
                                  color = "gray90"),
        axis.line = element_line(color = "gray"), 
        axis.ticks = element_line(color = "gray"))

