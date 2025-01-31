pacman::p_load(
  jmastats,
  tidyverse,
  here,
  crayon)



## list of 60 Rds files
rds.files <- dir(here("data/rds_update")) |> 
  as_tibble() |> 
  separate(col = value,
           into = c("hd", "area", "tl"),
           sep = "_", remove = FALSE)


## make stations list
Area.list <- read.csv(here("data", "Area_list.csv"))

Stations <- stations |>
  select(area, station_name, block_no) |> 
  left_join(Area.list, by = c("area" = "Area_jp")) |> 
  group_by(block_no) |> 
  distinct(block_no, .keep_all = TRUE) |>
  ungroup()
  
  
for(i in 1:nrow(rds.files)){
  BLK_NO  <- Stations |> 
    filter(Area_en == rds.files$area[i]) |> 
    pull(block_no)
  
  CHECK <- readRDS(here("data/rds_update", rds.files$value[i])) |> 
    pull(block_no) |> 
    unique() |> 
    as_tibble() |> 
    rowwise() |> 
    mutate(flg = ifelse(value %in% BLK_NO, 1, 0)) 
    
  if(sum(CHECK$flg) == nrow(CHECK)){
    cat(blue(paste0(i, ": ", rds.files$value[i], ": OK\n")))
  } else {
    cat(red(paste0(i, ": ", rds.files$value[i], ": NG!!!\n")))
  }
}
