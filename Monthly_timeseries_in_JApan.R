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
  slider,
  supportR,
  jmastats)



########################################
# GitHubからデータを読み込み
# 月毎の平均気温を算出
#
# 15地点のデータ抽出
#
########################################

STATION <- c("47409", "47420", "47421", "47588", "47592",
             "47606", "47637", "47648", "47742", "47755",
             "47761", "47830", "47890", "47909", "47918")


GITHUB.raw01 <- "https://raw.githubusercontent.com/igproj-fusion/JMAstats2/"
GITHUB.raw02 <- "main/data/rds_update/"
GITHUB.area <- "refs/heads/main/data/Area_list.csv"

Area.jp <- stations |> 
  filter(block_no %in% STATION) |>
  distinct(block_no, .keep_all = TRUE) |> 
  pull(area)

area15 <- read.csv(paste0(GITHUB.raw01, GITHUB.area)) |> 
  filter(Area_jp %in% Area.jp) |> 
  pull(Area_en)


RDS.github <- github_ls(repo = "https://github.com/igproj-fusion/JMAstats2", 
          recursive = TRUE, quiet = FALSE) |> 
  filter(path == "./data/rds_update") |> 
  mutate(url = paste0(GITHUB.raw01, GITHUB.raw02, name)) |> 
  mutate(name2 = gsub("JMA_", "", name)) |> 
  separate(col = name2,
           into = c("area", "other"),
           sep = "_") |> 
  filter(area %in% area15) |> 
  pull(url)


df.org <- set_names(RDS.github) |>
  map(\(RDS) {
    readRDS(url(RDS, method = "libcurl")) |> 
      filter(block_no %in% STATION) |> 
      unnest(temperature) |> 
      clean_names() |> 
      mutate(Year = year(date),
             Month = month(date)) |>
      filter(Year >= 1898) |> 
      group_by(block_no, Year, Month) |> 
      summarize(Average = mean(average_c, na.rm = TRUE),
                Average.max = mean(max_c, na.rm = TRUE),
                Average.min = mean(min_c, na.rm = TRUE))
  }) |> 
  bind_rows() 

closeAllConnections()


########################################
# 各地点の平年値算出
########################################

REF <- df.org |> 
  filter(Year >= 1991 & Year <= 2020) |> 
  group_by(block_no, Month) |> 
  summarize(ref = mean(Average, na.rm = TRUE),
            ref.max = mean(Average.max, na.rm = TRUE),
            ref.min = mean(Average.min, na.rm = TRUE))


########################################
# 各地点の各月の偏差を算出して平均
# 各月の12ヶ月、132ヵ月移動平均を追加
########################################

ANOM <- df.org |> 
  left_join(REF, by = c("block_no", "Month")) |> 
  mutate(diff = Average - ref,
         diff.max = Average.max - ref.max,
         diff.min = Average.min - ref.min) |>
  group_by(Year, Month) |> 
  summarize(anom = mean(diff, na.rm = TRUE),
            anom.max = mean(diff.max, na.rm = TRUE),
            anom.min = mean(diff.min, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(mavg12 = slide_vec(anom, .f = mean, .before = 11),
         mavg12.max = slide_vec(anom.max, .f = mean, .before = 11),
         mavg12.min = slide_vec(anom.min, .f = mean, .before = 11),
         mavg132 = slide_vec(anom, .f = mean, .before = 131),
         mavg132.max = slide_vec(anom.max, .f = mean, .before = 131),
         mavg132.min = slide_vec(anom.min, .f = mean, .before = 131)) |> 
  mutate(mavg12 = lead(mavg12, 6),
         mavg12.max = lead(mavg12.max, 6),
         mavg12.min = lead(mavg12.min, 6),
         mavg132 = lead(mavg132, 66),
         mavg132.max = lead(mavg132.max, 66),
         mavg132.min = lead(mavg132.min, 66)) |>
  mutate(N = 1:n()) |> 
  mutate(mavg12 = ifelse(N <= 6, NA, mavg12),
         mavg12.max = ifelse(N <= 6, NA, mavg12.max),
         mavg12.min = ifelse(N <= 6, NA, mavg12.min),
         mavg132 = ifelse(N <= 66, NA, mavg132),
         mavg132.max = ifelse(N <= 66, NA, mavg132.max),
         mavg132.min = ifelse(N <= 66, NA, mavg132.min)) |> 
  mutate(date = Year + (Month - 1) / 12)


#########################################################
#

Ann.JMA <- "https://www.data.jma.go.jp/cpdinfo/temp/list/csv/an_jpn.csv"
jma.df <- read.csv(Ann.JMA, fileEncoding = "cp932") |> 
  rename(Year = 1,
         anom.ann = 2)


########################################
# プロット
########################################

YearMonth <- RDS.github[1] |> 
  as_tibble() |> 
  mutate(value = gsub(paste0(GITHUB.raw01, GITHUB.raw02, "JMA_"), 
                      "", value)) |> 
  mutate(value = gsub("\\.rds", "", value)) |> 
  separate(col = value,
           into = c("area", "YearMonth"),
           sep = "_") |> 
  pull(YearMonth)

Last.Year <- substr(YearMonth, 1, 4)
Last.Month <- month.abb[as.integer(substr(YearMonth, 5, 6))] 
Last.YM <- paste(Last.Month, Last.Year)

ggplot(ANOM) +
  geom_hline(yintercept = 0, color = "gray65") +
#  geom_line(aes(date, anom),
#            color = "gray70", linewidth = 0.1) +
  geom_point(data = jma.df, aes(Year + 0.5, anom.ann),
             color = "gray40") +
  geom_line(aes(date, mavg12), 
            color = "#2266ff90", linewidth = 1) +
  geom_line(aes(date, mavg132),
            color = "darkmagenta", linewidth = 1) +

  guides(x = guide_axis(minor.ticks = FALSE, cap = "both"),
         y = guide_axis(minor.ticks = TRUE, cap = "both")) +
  scale_x_continuous(breaks = seq(1900, 2025, 25)) +

  geom_segment(aes(x = 1900, y = 1.43, 
                   xend = 1906, yend = 1.43),
               color = "#2266ff90",
               linewidth = 1.0) +
  geom_segment(aes(x = 1900, y = 1.20, 
                   xend = 1906, yend = 1.20),
               color = "darkmagenta",
               linewidth = 1.0) +
  geom_point(aes(x = 1904, y = 0.97),
             color = "gray30",
             size = 1.2)  +
  
  annotate("text", x = 1907, y = 1.43, 
           label = "12-month Running Mean", 
           color = "gray20", size = 3.75,
           hjust = 0) +
  annotate("text", x = 1907, y = 1.20, 
           label = "132-month Running Mean", 
           color = "gray20", size = 3.75,
           hjust = 0) +
  annotate("text", x = 1907, y = 0.97, 
           label = "Annual Mean", 
           color = "gray20", size = 3.75,
           hjust = 0) +
  
  annotate("text", x = 2005, y = -2, 
           label = "Baseline: 1991-2020", 
           color = "gray20", size = 3.75,
           hjust = 0) +



  scale_y_continuous(breaks = seq(-2, 1.5, 0.5),
                     limits = c(-2, 1.5)) +
  labs(x = "", y = "Temperature Anomaly (°C)", color = "",
       subtitle = paste0("Jan 1898 ~ ", Last.YM),
       caption = "SOURCE: https://www.data.jma.go.jp/stats/etrn/index.php",
       title = "Monthly Mean Temperature Anomaly in Japan") +
  theme_light() +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(0.95)),
        axis.title.y = element_text(size = rel(1.1),
                                    color = "gray15",
                                    vjust = 1),
        axis.text  = element_text(size = rel(0.9),
                                  color = "gray10"))


