axis <- sf::st_read("data/eixo_osm+ippuc_lim_velocidade.gpkg")

cod <- 4106902
cwb <- geobr::read_municipality(code_muni = cod, year = 2010)

# test <- readxl::read_xlsx("data/Driver_G_fulltable_2021_09_30.xlsx") %>% 
#   suppressWarnings()

# test <- vroom::vroom("data/coleta2.csv", delim = ",")


driver_sf <- function(df) {
  df %>% 
    drop_na(LONG, LAT) %>% 
    st_as_sf(coords = c("LONG", "LAT")) %>% 
    st_set_crs(4674)
}

join_cwb <- function(df) {
  df %>% 
    st_join(cwb["name_muni"]) %>% 
    filter(name_muni == "Curitiba") %>% 
    select(-name_muni)
}

filter_axis <- function(axis, df) {
  bbox <- st_bbox(df) %>% 
    st_as_sfc() %>% 
    st_transform(crs = 31982)
  
  new_axis <- axis["limite_vel"] %>% 
    st_intersection(bbox)
}

join_limits <- function(df) {
  df %>% 
    st_transform(crs = 31982) %>% 
    st_join(st_buffer(axis["limite_vel"], dist = 10)) %>% 
    filter(!is.na(limite_vel)) %>% 
    distinct(DAY, PR, .keep_all = TRUE) %>% 
    st_transform(crs = 4674)
}

speeding_calc <- function(df) {
  df %>% 
    cbind(., st_coordinates(.)) %>% 
    st_drop_geometry() %>% 
    mutate(
      EXP = if_else(limite_vel - SPD_KMH < 10, "Yes", "No"),
      SPD_10 = if_else(SPD_KMH > limite_vel * 1.1, "Yes", "No"),
      SPD_20 = if_else(SPD_KMH > limite_vel * 1.2, "Yes", "No"),
      SPD_30 = if_else(SPD_KMH > limite_vel * 1.3, "Yes", "No")
    ) %>% 
    rename(SPD_LIMIT = limite_vel)
}

# test <- test %>% 
#   driver_sf() %>% 
#   join_cwb()
# 
# axis <- filter_axis(axis, test)
# 
# test <- test %>% 
#   join_limits() %>% 
#   speeding_calc()
