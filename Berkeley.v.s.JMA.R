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
      group_by(block_no, Year, Month) |> 
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
  group_by(block_no, Month) |> 
  summarize(ref = mean(Average, na.rm = TRUE),
            ref.max = mean(Average.max, na.rm = TRUE),
            ref.min = mean(Average.min, na.rm = TRUE))


########################################
# 各地点の各年の偏差を算出して平均
# 各年の5年移動平均を追加
########################################

ANOM <- df.org |> 
  left_join(REF, by = c("block_no", "Month")) |> 
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
             color = "gray80", size = 1.8) +
  geom_line(aes(Year, mavg5), color = "cornflowerblue", linewidth = 0.6) +
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



#################################################################
#
# Berkeley Earth, GISTEMPv4 / Climate Pulse : Monthly Data
#   https://berkeleyearth.org/
#
#+    Select JAPAN > VIEW MORE DETAIL > Data Table
#+      https://berkeley-earth-projections.s3.us-west-1.amazonaws.com/policy-forecasts/Japan-projection.txt
#+      
#
# ※2025/04/04 
#
#################################################################

URL0 <- "https://berkeley-earth-projections.s3.us-west-1.amazonaws.com/"
URL1 <- "policy-forecasts/Japan-projection.txt"
BerkDat <- read.table(paste0(URL0, URL1), 
                      comment.char = "%", sep = "") |> 
  filter(V1 >= 1898) |> 
  select(Year = V1, temp = V2) |> 
  filter(!is.na(temp))

Berk.ref <- BerkDat |> 
  filter(Year >= 1991 & Year <= 2020) |> 
  summarize(ref = mean(temp, na.rm = TRUE)) |> 
  pull(ref)

Berk.anom <- BerkDat |> 
  mutate(anom = temp - Berk.ref)



Berk.URL <- "https://berkeleyearth.org/"
JMA.URL <- "https://www.data.jma.go.jp/cpdinfo/temp/an_jpn.htm"

ggplot() +
  geom_hline(yintercept = 0, color = "gray55") +
  geom_line(data = ANOM,
            aes(Year, Anomaly), alpha = 0.5,
            color = "blue", linewidth = 0.1,
            linetype = "dashed") +
  geom_point(data = ANOM,
            aes(Year, Anomaly), alpha = 0.5,
            color = "blue", size = 2) +
  geom_line(data = ANOM,
            aes(Year, Anomaly + Berk.ref),
            color = "blue", linewidth = 0.1) +
  geom_point(data = ANOM,
             aes(Year, Anomaly + Berk.ref),
             color = "blue", size = 2) +
  geom_line(data = Berk.anom,
             aes(Year, temp),
             color = "darkmagenta", linewidth = 0.1) +
  geom_point(data = Berk.anom,
            aes(Year, temp),
            color = "darkmagenta", size = 2.5) +
  
  geom_segment(aes(x = 1900, y = 3.2, xend = 1908, yend = 3.2),
               color = "blue", linetype = "dotted",
               linewidth = 0.75) +
  geom_point(aes(x = 1904, y = 3.2),
             color = "blue",
             size = 1.8) +
  annotate("text", x = 1909, y = 3.2, label="JMA (relative to 1991-2020)",
           hjust = 0, vjust = 0.5, size = 4.15, color = "gray30",
           family= "Georgia") +
  
  geom_segment(aes(x = 1900, y = 2.8, xend = 1908, yend = 2.8),
               color = "blue", 
               linewidth = 0.75) +
  geom_point(aes(x = 1904, y = 2.8),
             color = "blue",
             size = 1.8) +
  annotate("text", x = 1909, y = 2.8, label="JMA (relative tp late 19th century)",
           hjust = 0, vjust = 0.5, size = 4.15, color = "gray30",
           family= "Georgia") +

  geom_segment(aes(x = 1900, y = 2.4, xend = 1908, yend = 2.4),
               color = "darkmagenta", 
               linewidth = 0.75) +
  geom_point(aes(x = 1904, y = 2.4),
             color = "darkmagenta",
             size = 1.8) +
  annotate("text", x = 1909, y = 2.4, label="Berkeley (relative tp late 19th century)",
           hjust = 0, vjust = 0.5, size = 4.15, color = "gray30",
           family= "Georgia") +
  
  scale_x_continuous(breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(breaks = seq(-2, 3., .5),
                     limits = c(-2.1, 3.35)) +
  
  labs(x = "", y = "Temperature Anomaly (°C)", color = "",
       caption = paste0(Berk.URL, "\n", JMA.URL),
       title = "Warming in Japan") +
  theme_bw() +
  theme(text=element_text(size=12,  family="serif")) +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor.y = element_line(colour="white", size=0.5),
        #axis.line = element_line(color = "gray30", linewidth = rel(1)),
        plot.title = element_text(size = rel(1.25)),
        plot.subtitle = element_text(size = rel(1.1)),
        plot.caption = element_text(size = rel(1.1),
                                    color = "gray35"),
        axis.text = element_text(size = rel(1.0)),
        axis.title.y = element_text(size = rel(1.15), 
                                  color = "gray35", vjust = 2),
        legend.position = c(0.1, 0.825))



  

