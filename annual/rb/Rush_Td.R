library(tidyverse)
library(nflfastR)

Rush_td <- function(year, rusher_player_id){
  pbp <- load_pbp(year)
  
  pbp_r <- pbp |>
    filter(season_type == "REG",
           rush_touchdown == 1,                
           !is.na(rusher_player_id),
           rusher_player_id == !!rusher_player_id,
           td_player_id == rusher_player_id)
  
  if (nrow(pbp_r) == 0) {
    return(data.frame(
      id = rusher_player_id,
      rushing_touchdowns = 0
    ))
  }
  
  total_rush_td <- nrow(pbp_r)
  nombre <- unique(pbp_r$rusher_player_name)
  
  
  return(data.frame(
    nombre = nombre,
    rushing_touchdowns = total_rush_td
  ))
}

# prueba
# pbp_2024_td_rush <- load_pbp(2024) |>
#   filter(season_type == "REG",
#          rush_touchdown == 1,
#          !is.na(rusher_player_id),
#          td_player_id == rusher_player_id)
# 
# ids_con_rush_td <- pbp_2024_td_rush |>
#   distinct(rusher_player_id) |>
#   pull(rusher_player_id)
# 
# resultados <- map_dfr(ids_con_rush_td, ~ Rush_td(2024, .x))
# 
# resultados_ordenado <- resultados |> arrange(desc(touchdowns))

