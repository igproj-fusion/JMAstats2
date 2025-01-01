#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ホーム > 各種データ・資料 > 地球環境・気候 > 地球温暖化 > 
#   気温・降水量の長期変化傾向 > 日本の年平均気温
#   https://www.data.jma.go.jp/cpdinfo/temp/an_jpn.html
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
# 年毎の平均気温を算出
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
      filter(block_no %in% STATION) |> 
      unnest(temperature) |> 
      clean_names() |> 
      mutate(Year = year(date),
             Month = month(date)) |>
      filter(Year >= 1898 & Year <= 2024) |> 
      group_by(block_no, Year) |> 
      summarize(Average = mean(average_c, na.rm = TRUE),
                Average.max = mean(max_c, na.rm = TRUE),
                Average.min = mean(min_c, na.rm = TRUE))
  }) |> 
  bind_rows() 

closeAllConnections()


########################################
# 各地点の年ごとの平年値算出
########################################

REF <- df.org |> 
  filter(Year >= 1991 & Year <= 2020) |> 
  group_by(block_no) |> 
  summarize(ref = mean(Average, na.rm = TRUE),
            ref.max = mean(Average.max, na.rm = TRUE),
            ref.min = mean(Average.min, na.rm = TRUE))


########################################
# 各地点の各年の偏差を算出して平均
# 各年の5年移動平均を追加
########################################

ANOM <- df.org |> 
  left_join(REF, by = "block_no") |> 
  mutate(diff = Average - ref,
         diff.max = Average.max - ref.max,
         diff.min = Average.min - ref.min) |>
  group_by(Year) |> 
  summarize(Anomaly = mean(diff, na.rm = TRUE),
            Anomaly.max = mean(diff.max, na.rm = TRUE),
            Anomaly.min = mean(diff.min, na.rm = TRUE)) |>  
  mutate(mavg5 = slide_vec(Anomaly, .f = mean, .before = 4),
         mavg5.max = slide_vec(Anomaly.max, .f = mean, .before = 4),
         mavg5.min = slide_vec(Anomaly.min, .f = mean, .before = 4)) |> 
  mutate(mavg5 = lead(mavg5, 2),
         mavg5.max = lead(mavg5.max, 2),
         mavg5.min = lead(mavg5.min, 2)) |> 
  mutate(mavg5 = ifelse(Year < 1900, NA, mavg5),
         mavg5.max = ifelse(Year < 1900, NA, mavg5.max),
         mavg5.min = ifelse(Year < 1900, NA, mavg5.min))


########################################
# 回帰係数算出
########################################

res <- lm(Anomaly ~ Year, data = ANOM) 
coef = round2(res$coefficients[2] * 100, 2) 
res.max <- lm(Anomaly.max ~ Year, data = ANOM) 
coef.max = round2(res.max$coefficients[2] * 100, 2) 
res.min <- lm(Anomaly.min ~ Year, data = ANOM) 
coef.min = round2(res.min$coefficients[2] * 100, 2) 


########################################
# プロット
########################################

ggplot(ANOM) +
  geom_hline(yintercept = 0, color = "gray95") +
  geom_line(aes(Year, Anomaly),
            color = "gray80", linewidth = 0.5) +
  geom_point(aes(Year, Anomaly),
             color = "gray80", size = 2.2) +
  geom_line(aes(Year, mavg5), color = "cornflowerblue", linewidth = 1.0) +
  geom_smooth(aes(Year, Anomaly),
              method = lm, se = FALSE, color = "red") +
  guides(x = guide_axis(minor.ticks = FALSE, cap = "both"),
         y = guide_axis(minor.ticks = TRUE, cap = "both")) +
  scale_x_continuous(breaks = c(1898, seq(1910, 2010, 10), 2024)) +
  annotate("text", x = 1918, y = 1.75, 
           label = paste("Trend ＝", format(coef, nsmall = 2), "(°C/100years)"), 
           color = "gray90", size = 4.) +
  ylim(c(-2, 2)) +
  theme_hc(style = "darkunica") +
  labs(x = "", y = "Anomaly (°Celsius)",
       title = "Average Temperature Anomaly") + 
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1), color = "gray90"),
        axis.text  = element_text(color = "gray90"),
        axis.line = element_line(color = "gray"), 
        axis.ticks = element_line(color = "gray"))

ggplot(ANOM) +
  geom_hline(yintercept = 0, color = "gray95") +
  geom_line(aes(Year, Anomaly.max),
            color = "gray80", linewidth = 0.5) +
  geom_point(aes(Year, Anomaly.max),
             color = "gray80", size = 2.2) +
  geom_line(aes(Year, mavg5.max), color = "cornflowerblue", linewidth = 1.0) +
  geom_smooth(aes(Year, Anomaly.max),
              method = lm, se = FALSE, color = "red") +
  guides(x = guide_axis(minor.ticks = FALSE, cap = "both"),
         y = guide_axis(minor.ticks = TRUE, cap = "both")) +
  scale_x_continuous(breaks = c(1898, seq(1910, 2010, 10), 2024)) +
  annotate("text", x = 1918, y = 1.75, 
           label = paste("Trend ＝", format(coef.max, nsmall = 2), "(°C/100years)"), 
           color = "gray90", size = 4.) +
  ylim(c(-2, 2)) +
  theme_hc(style = "darkunica") +
  labs(x = "", y = "Anomaly (°Celsius)",
       title = "Average Max.Temperature Anomaly") + 
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1), color = "gray90"),
        axis.text  = element_text(color = "gray90"),
        axis.line = element_line(color = "gray"), 
        axis.ticks = element_line(color = "gray"))

ggplot(ANOM) +
  geom_hline(yintercept = 0, color = "gray95") +
  geom_line(aes(Year, Anomaly.min),
            color = "gray80", linewidth = 0.5) +
  geom_point(aes(Year, Anomaly.min),
             color = "gray80", size = 2.2) +
  geom_line(aes(Year, mavg5.min), color = "cornflowerblue", linewidth = 1.0) +
  geom_smooth(aes(Year, Anomaly.min),
              method = lm, se = FALSE, color = "red") +
  guides(x = guide_axis(minor.ticks = FALSE, cap = "both"),
         y = guide_axis(minor.ticks = TRUE, cap = "both")) +
  scale_x_continuous(breaks = c(1898, seq(1910, 2010, 10), 2024)) +
  annotate("text", x = 1918, y = 1.75, 
           label = paste("Trend ＝", format(coef.min, nsmall = 2), "(°C/100years)"), 
           color = "gray90", size = 4.) +
  ylim(c(-2.5, 2)) +
  theme_hc(style = "darkunica") +
  labs(x = "", y = "Anomaly (°Celsius)",
       title = "Average Min.Temperature Anomaly") + 
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1), color = "gray90"),
        axis.text  = element_text(color = "gray90"),
        axis.line = element_line(color = "gray"), 
        axis.ticks = element_line(color = "gray"))



