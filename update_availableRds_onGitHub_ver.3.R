######################################################
#
# 月が変わって更新するときは、
#
# local DiskのRprojectフォルダ配下のJMAstats2/data/rds_update
# このフォルダの既存rdsファイルは削除される。
# 先月分のデータが追加された新規rdsファイルがここに保存される。
#
# 更新が終了したら全データを確認した後、問題が無ければ
# GitHub/JMAstats2にpushする。
#
######################################################


pacman::p_load(
  jmastats,
  units,
  tidyverse,
  janitor,
  here,
  tools,
  stringi,
  bazar,
  supportR)


LOCAL_repo = "data/rds_update"
files <- dir(here(LOCAL_repo))
if (length(files) != 0) {
  file.remove(here(LOCAL_repo, files))
}


GITHUB.raw = "https://raw.githubusercontent.com/igproj-fusion/"
GITHUB.repo = "JMAstats2/main/data/rds_update/"


RDS.github <- github_ls(repo = "https://github.com/igproj-fusion/JMAstats2", 
                        recursive = TRUE, quiet = FALSE) |> 
  filter(path == "./data/rds_update") |> 
  mutate(url = paste0(GITHUB.raw, GITHUB.repo, name)) |> 
  select(name, url) |> 
  separate(col = name,
           into = c("h1", "area", "h2"),
           sep = "_") |> 
  select(area, url)

YearMonth <- substr(RDS.github$url[1], 
       str_length(RDS.github$url[1]) - 9, 
       str_length(RDS.github$url[1]) - 4)

Year <- as.numeric(substr(YearMonth, 1, 4))                      
Month <- as.numeric(substr(YearMonth, 5, 6))

if(Month == 12){
  Year <- Year + 1
  Month <- 1
} else {
  Month <- Month + 1
}
Month <- formatC(Month, width = 2, flag = "0")

for(i in 1:length(RDS.github)) {
  
  df.org <- readRDS(url(RDS.github$url[i], method = "libcurl"))
  
  Area = RDS.github$area[i]
  
  BLOCK <- df.org |> 
    select(block_no) |> 
    distinct(block_no, .keep_all = TRUE) |> 
    pull(block_no)
  
  df <- set_names(BLOCK) |> 
    map(\(BLOCK_NO) 
        jma_collect(item = "daily",
                    block_no = BLOCK_NO,
                    year = Year, 
                    month = Month, 
                    cache = FALSE,
                    quiet = TRUE) |> 
          mutate(block_no = BLOCK_NO) |>
          mutate(PREFECTURE = Area)) |> 
    bind_rows()
  
  Rds.name <- paste0("JMA_",  Area,
                     "_", Year, Month, ".rds")
  saveRDS(bind_rows(df.org, df), 
          file = here(LOCAL_repo, Rds.name))
}  



