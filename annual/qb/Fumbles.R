library(tidyverse)
library(nflfastR)

total_fumbles_lost <- function(year, player_id){
  pbp <- load_pbp(year)
  
  pbp_f <- pbp |> 
    filter(season_type == "REG") |>
    filter(fumble_lost == 1) |>
    filter(fumbled_1_player_id == !!player_id) |> 
    filter(!is.na(epa))
  
  if (nrow(pbp_f) == 0) {
    return(data.frame(
      id = player_id,
      fumbles_perdidos = 0
    ))
  }
  
  total_fumbles <- nrow(pbp_f)
  nombre <- unique(pbp_f$fumbled_1_player_name)
  
  return(data.frame(
    nombre = nombre,
    fumbles_perdidos = total_fumbles
  ))
}

#Prueba
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", fumble_lost == 1, !is.na(fumbled_1_player_id))
# 
# ids_jugadores <- unique(pbp_2024$fumbled_1_player_id)
# 
# fumbles_lost_df <- map_dfr(ids_jugadores, ~ total_fumbles_lost(2024, .x))
# 
# resultados_fum_lost <- fumbles_lost_df |> arrange(desc(fumbles_perdidos))
