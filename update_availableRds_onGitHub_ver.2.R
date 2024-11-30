######################################################
#
# add 2024 data
# ONE PREFECURE BULK
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
  bazar)


GITHUB.repo = "Rds202410"


GITHUB.raw = "https://raw.githubusercontent.com/igproj-fusion/JMAstats/main/"
Area.li <- read.csv(paste0(GITHUB.raw, "Area_list.csv"))


                      
RDS.url <- paste0(GITHUB.raw, 
                    GITHUB.repo, "/JMA_", Area.li$Area_en, "_",
                    gsub("Rds", "", GITHUB.repo), ".rds")                      
                      
Year <- as.numeric(substr(GITHUB.repo, 4, 7))                      
Month <- as.numeric(substr(GITHUB.repo, 8, 9))

if(Month == 12){
  Year <- Year + 1
  Month <- 1
} else {
  Month <- Month + 1
}
  

for(i in 1:length(RDS.url)) {
  
  df.org <- readRDS(url(RDS.url[i], method = "libcurl"))
  
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
          mutate(PREFECTURE = Area.li$Area_en[i])) |> 
    bind_rows()
  
  Rds.name <- paste0("JMA_",  Area.li$Area_en[i],
                     "_", Year, Month, ".rds")
  saveRDS(bind_rows(df.org, df), 
          file = here("data/rds_update", Rds.name))
}  



