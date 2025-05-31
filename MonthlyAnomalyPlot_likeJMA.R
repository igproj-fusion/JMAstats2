#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ホーム > 各種データ・資料 > 地球環境・気候 > 地球温暖化 > 
#   気温・降水量の長期変化傾向 > 日本の月平均気温
#   https://www.data.jma.go.jp/cpdinfo/temp/jan_jpn.html
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
# GitHubからデータを読み込み
# 月毎の平均気温を算出
#
# 15地点のデータ抽出
#
########################################

GITHUB.raw <- "https://raw.githubusercontent.com/igproj-fusion/JMAstats2/main/data/rds_update/"
RDS.github <- github_ls(repo = "https://github.com/igproj-fusion/JMAstats2", 
                        recursive = TRUE, quiet = FALSE) |> 
  filter(path == "./data/rds_update") |> 
  mutate(name = paste0(GITHUB.raw, name)) |> 
  pull(name)

STATION <- c("47409", "47420", "47421", "47588", "47592",
             "47606", "47637", "47648", "47742", "47755",
             "47761", "47830", "47890", "47909", "47918")

df.org <- set_names(RDS.github) |>
  map(\(RDS) {
    readRDS(url(RDS, method = "libcurl")) |> 
      filter(block_no %in% STATION & year(date) >= 1898) |> 
      unnest(temperature) |> 
      clean_names() |> 
      mutate(Year = year(date),
             Month = month(date)) |> 
      group_by(block_no, Year, Month) |> 
      summarize(Average = mean(average_c, na.rm = TRUE))
  }) |> 
  bind_rows() 
   
closeAllConnections()


########################################
# 各地点の月ごとの平年値算出
########################################

REF <- df.org |> 
  filter(Year >= 1991 & Year <= 2020) |> 
  group_by(block_no, Month) |> 
  summarize(ref = mean(Average, na.rm = TRUE))


########################################
# 各地点の各年各月の偏差を算出して平均
# 各月の5年移動平均を追加
########################################

ANOM <- df.org |> 
  left_join(REF, by = c("block_no", "Month")) |> 
  mutate(diff = Average - ref) |>
  group_by(Year, Month) |> 
  summarize(Anomaly = mean(diff, na.rm = TRUE)) |>  
  group_by(Month) |> 
  mutate(mavg5 = slide_vec(Anomaly, .f = mean, .before = 4)) |> 
  mutate(mavg5 = lead(mavg5, 2)) |> 
  mutate(mavg5 = ifelse(Year < 1900, NA, mavg5))
  

########################################
# 各月の回帰係数算出
########################################

Trend <- ANOM |> 
  group_by(Month) |> 
  do(trend = lm(Anomaly ~ Year, data = .)) |> 
  mutate(coef = round2(trend$coefficients[2] * 100, 2)) 


########################################
# 月を指定してプロット
########################################

MONTH = 4
ANOM.mon <- ANOM |> filter(Month == MONTH)
BREAKS <- c(1898, seq(1910, 2010, 10), max(ANOM.mon$Year))
TITLE <- paste0("Average Temperature Anomaly: ", month.name[MONTH])

ggplot(ANOM.mon, aes(Year, Anomaly)) +
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

