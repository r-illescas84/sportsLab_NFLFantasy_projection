library(tidyverse)
library(nflfastR)

Rush_att_total <- function(year, rusher_player_id){
  pbp <- load_pbp(year)
  
  pbp_r <- pbp |> 
    filter(season_type == "REG") |>
    filter(rush_attempt == 1) |> 
    filter(rusher_player_id == !!rusher_player_id) |> 
    filter(!is.na(epa)) #Filtra penalty plays
  
  if (nrow(pbp_r) == 0) {
    return(data.frame(
      id = rusher_player_id,
      attempts = 0
    ))
  }
  
  total_attempts <- nrow(pbp_r)
  nombre <- unique(pbp_r$rusher_player_name)
  
  
  return(data.frame(
    nombre = nombre,
    attempts = total_attempts
  ))
}

#Prueba con todos los jugadores con al menos 100 rushing attempt en 2024
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", rush_attempt == 1) |>
#   filter(!is.na(rusher_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(rusher_player_id, name = "attempts") |>
#   filter(attempts >= 100) |>
#   pull(rusher_player_id)
# 
# resultadosr <- map_dfr(ids_filtrados, ~ Rush_att_total(2024, .x))
# 
# resultados_ordenador <- resultadosr |> arrange(desc(attempts))
